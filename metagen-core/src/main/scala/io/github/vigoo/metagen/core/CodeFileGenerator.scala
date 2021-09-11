package io.github.vigoo.metagen.core

import org.scalafmt.interfaces.Scalafmt
import zio.blocking._
import zio.nio.core.file.Path
import zio.nio.file.Files
import zio.{Chunk, Has, IO, Ref, UIO, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.meta._
import scala.meta.contrib.XtensionTreeEquality
import scala.meta.internal.prettyprinters.TreeSyntax

trait CodeFileGenerator {
  def targetPath: UIO[Path]
  def writeIfDifferent(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], Unit]
  def prettyPrint(tree: Tree): IO[GeneratorFailure[Nothing], String]
  def format(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], String]
  def seal(tree: Term.Block): ZIO[Any, GeneratorFailure[Nothing], Tree]
}

object CodeFileGenerator {
  class Live(contextRef: Ref[CodeFileGeneratorContext], scalafmt: Scalafmt) extends CodeFileGenerator {

    def targetPath: UIO[Path] =
      contextRef.get.map(context =>
        context.globalContext.root / context.currentPackage.asPath / (context.name + ".scala")
      )

    def writeIfDifferent(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], Unit] =
      targetPath.flatMap { path =>
        Files.exists(path).flatMap { exists =>
          for {
            existingBytes <-
              if (exists) {
                Files.readAllBytes(path).mapError(GeneratorFailure.FailedToReadFile)
              } else {
                ZIO.succeed(Chunk.empty)
              }
            contentsBytes  =
              Chunk.fromArray(contents.getBytes(StandardCharsets.UTF_8))
            _             <-
              Files
                .writeBytes(path, contentsBytes)
                .mapError(GeneratorFailure.FailedToWriteFile)
                .unless(existingBytes == contentsBytes)
          } yield ()
        }
      }

    def prettyPrint(tree: Tree): IO[GeneratorFailure[Nothing], String] =
      contextRef.get.flatMap { context =>
        val dialect =
          if (context.globalContext.scalaVersion.startsWith("3.")) scala.meta.dialects.Scala3
          else if (context.globalContext.scalaVersion.startsWith("2.13.")) scala.meta.dialects.Scala213
          else scala.meta.dialects.Scala212

        ZIO
          .effect {
            val prettyPrinter = TreeSyntax[Tree](dialect)
            prettyPrinter(tree).toString
          }
          .mapError(GeneratorFailure.PrettyPrintingFailure)
      }

    def format(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], String] =
      targetPath
        .flatMap { path =>
          effectBlocking {
            scalafmt.format(Paths.get(".scalafmt.conf"), path.toFile.toPath, contents)
          }
        }
        .mapError(GeneratorFailure.ScalaFmtFailure)

    def seal(tree: Term.Block): ZIO[Any, GeneratorFailure[Nothing], Tree] =
      contextRef.get.flatMap { context =>
        ZIO
          .effect {
            val usedTypes =
              tree
                .collect {
                  case t: Type.Select if t.qual.isEqual(q"scala") || t.qual.isEqual(q"scala.Predef") =>
                    (t.qual, t.name)
                }
                .groupBy(_._1)

            val finalState = usedTypes.foldLeft(OptimizationState.initial) { case (state, (ref, names)) =>
              names.foldLeft(state) { case (state, (_, name)) =>
                if (state.usedNames.contains(name.value)) {
                  // This type name is already in scope, we have to keep it fully qualified in the source
                  state.copy(
                    keepFullyQualified = state.keepFullyQualified + Type.Select(ref, name).toString()
                  )
                } else {
                  // This type name is not in scope yet, we can import it
                  state.copy(
                    usedNames = state.usedNames + name.value,
                    imports = Import(
                      List(Importer(ref, names.map { case (_, name) => Importee.Name(Name(name.value)) }))
                    ) :: state.imports
                  )
                }
              }
            }

            val transformedTree =
              tree.transform {
                case t: Type.Select if !finalState.keepFullyQualified.contains(t.toString()) =>
                  t.name
              }.asInstanceOf[Term.Block]

            Pkg(
              context.currentPackage.term,
              finalState.imports ++ transformedTree.stats
            )
          }
          .mapError(GeneratorFailure.TreeTransformationFailure)
      }
  }

  case class OptimizationState(usedNames: Set[String], keepFullyQualified: Set[String], imports: List[Import])
  object OptimizationState extends KnownTypes {
    val initial: OptimizationState =
      OptimizationState(
        usedNames = predefinedTypeNames,
        keepFullyQualified = Set.empty,
        imports = Nil
      )
  }

  def make(scalafmt: Scalafmt, globalContext: GeneratorContext, pkg: Package, name: String): UIO[CodeFileGenerator] =
    for {
      contextRef <- Ref.make(CodeFileGeneratorContext(globalContext, pkg, name))
    } yield new Live(contextRef, scalafmt)

  def targetPath: ZIO[Has[CodeFileGenerator], Nothing, Path] =
    ZIO.serviceWith(_.targetPath)

  def writeIfDifferent(contents: String): ZIO[Blocking with Has[CodeFileGenerator], GeneratorFailure[Nothing], Unit] =
    ZIO.service[CodeFileGenerator].flatMap(_.writeIfDifferent(contents))

  def prettyPrint(tree: Tree): ZIO[Has[CodeFileGenerator], GeneratorFailure[Nothing], String] =
    ZIO.serviceWith(_.prettyPrint(tree))

  def format(contents: String): ZIO[Blocking with Has[CodeFileGenerator], GeneratorFailure[Nothing], String] =
    ZIO.service[CodeFileGenerator].flatMap(_.format(contents))

  def seal(tree: Term.Block): ZIO[Has[CodeFileGenerator], GeneratorFailure[Nothing], Tree] =
    ZIO.serviceWith(_.seal(tree))
}
