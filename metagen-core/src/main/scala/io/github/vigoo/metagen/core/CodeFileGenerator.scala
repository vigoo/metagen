package io.github.vigoo.metagen.core

import org.scalafmt.interfaces.Scalafmt
import zio.blocking._
import zio.nio.core.file.Path
import zio.nio.file.Files
import zio.{Chunk, Has, IO, Ref, UIO, ZIO, console}

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib.XtensionTreeEquality
import scala.meta.internal.prettyprinters.TreeSyntax

trait CodeFileGenerator {
  def targetPath: UIO[Path]
  def writeIfDifferent(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], Unit]
  def prettyPrint(tree: Tree): IO[GeneratorFailure[Nothing], String]
  def format(contents: String): ZIO[Blocking, GeneratorFailure[Nothing], String]
  def seal(tree: Term.Block): IO[GeneratorFailure[Nothing], Tree]
  def knownLocalName(name: String): IO[Nothing, Unit]
}

object CodeFileGenerator {
  class Live(contextRef: Ref[CodeFileGeneratorContext], scalafmt: Scalafmt) extends CodeFileGenerator {

    def targetPath: UIO[Path] =
      contextRef.get.map(context =>
        context.target match {
          case CodeFileGeneratorContext.ScalaPackage(pkg, name)             =>
            context.globalContext.root / pkg.asPath / (name + ".scala")
          case CodeFileGeneratorContext.ScalaPackageObject(parentPkg, name) =>
            context.globalContext.root / parentPkg.asPath / name / "package.scala"
          case CodeFileGeneratorContext.RawFile(relativePath)               =>
            context.globalContext.root / relativePath
        }
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

    def knownLocalName(name: String): IO[Nothing, Unit] =
      contextRef.update { context =>
        context.copy(knownNames = context.knownNames + name)
      }

    def seal(tree: Term.Block): ZIO[Any, GeneratorFailure[Nothing], Tree] =
      contextRef.get.flatMap { context =>
        ZIO
          .effect {
            val usedTypes =
              tree
                .collect {
                  case Type.Select(qual, name) =>
                    List((qual, name))
                  case Type.Apply(t, ts)       =>
                    (t :: ts).collect { case Type.Select(qual, name) =>
                      (qual, name)
                    }
                }
                .flatten
                .groupBy { case (ref, _) => new WrappedRef(ref) }
                .mapValues(_.map(_._2.value).toSet)
                .filterKeys(r => !r.ref.isEqual(Package.scala.term))
                .filterKeys(r => !r.ref.isEqual(Package.predef.term))

            val definedNames =
              tree.collect {
                case Defn.Class(_, name, _, _, _) => name.value
                case Defn.Trait(_, name, _, _, _) => name.value
              }.toSet union context.knownNames

            val finalState = usedTypes.foldLeft(OptimizationState.initial(definedNames)) { case (state, (ref, names)) =>
              names.foldLeft(state) { case (state, name) =>
                if (state.usedNames.contains(name)) {
                  // This type name is already in scope, we have to keep it fully qualified in the source
                  state.copy(
                    keepFullyQualified = state.keepFullyQualified + Type.Select(ref.ref, Type.Name(name)).toString()
                  )
                } else {
                  // This type name is not in scope yet, we can import it
                  state.copy(
                    usedNames = state.usedNames + name,
                    imports = Import(
                      List(Importer(ref.ref, List(Importee.Name(Name(name)))))
                    ) :: state.imports
                  )
                }
              }
            }

            val transformedTree =
              tree
                .transform {
                  case t: Type.Select
                      if !isInImportStatement(t) && !finalState.keepFullyQualified.contains(t.toString()) && finalState
                        .importsType(t) =>
                    t.name

                  case t: Term.Select =>
                    t.qual match {
                      case tt: Term.Select
                          if !isInImportStatement(t) && !finalState.keepFullyQualified.contains(
                            tt.toString()
                          ) && finalState.importsTerm(tt) =>
                        Term.Select(tt.name, t.name)
                      case _ => t
                    }
                }
                .asInstanceOf[Term.Block]

            context.target match {
              case CodeFileGeneratorContext.ScalaPackage(pkg, _)                =>
                Pkg(
                  pkg.term,
                  finalState.collapsedImports(Some(pkg)) ++ transformedTree.stats
                )
              case CodeFileGeneratorContext.ScalaPackageObject(parentPkg, name) =>
                Pkg(
                  parentPkg.term,
                  finalState.collapsedImports(Some(parentPkg)) :+
                    Pkg.Object(
                      mods = Nil,
                      name = Term.Name(name),
                      templ = Template(
                        early = Nil,
                        inits = Nil,
                        self = Self(Name.Anonymous(), None),
                        stats = transformedTree.stats
                      )
                    )
                )

              case CodeFileGeneratorContext.RawFile(_) =>
                Term.Block(finalState.collapsedImports(None) ++ transformedTree.stats)
            }
          }
          .mapError(GeneratorFailure.TreeTransformationFailure)
      }

    @tailrec
    private def isInImportStatement(t: Tree): Boolean =
      t.parent match {
        case Some(Import(_)) => true
        case Some(parent)    => isInImportStatement(parent)
        case None            => false
      }
  }

  case class OptimizationState(usedNames: Set[String], keepFullyQualified: Set[String], imports: List[Import]) {
    lazy val importedTypes: Set[Type.Select] =
      (for {
        imp                <- imports
        importer           <- imp.importers
        importee           <- importer.importees
        Importee.Name(name) = importee
      } yield Type.Select(importer.ref, Type.Name(name.value))).toSet

    lazy val importedTerms: Set[Term.Select] =
      (for {
        imp                <- imports
        importer           <- imp.importers
        importee           <- importer.importees
        Importee.Name(name) = importee
      } yield Term.Select(importer.ref, Term.Name(name.value))).toSet

    def importsType(t: Type.Select): Boolean =
      importedTypes.exists(r => r.isEqual(t)) ||
        t.qual.isEqual(Package.scala.term) ||
        t.qual.isEqual(Package.predef.term)

    def importsTerm(t: Term.Select): Boolean =
      importedTerms.exists(r => r.isEqual(t)) ||
        t.qual.isEqual(Package.scala.term) ||
        t.qual.isEqual(Package.predef.term)

    def collapsedImports(localPackage: Option[Package]): List[Import] =
      imports
        .flatMap(_.importers)
        .groupBy(i => new WrappedRef(i.ref))
        .toList
        .filter { case (key, _) =>
          localPackage match {
            case Some(pkg) => !pkg.term.isEqual(key.ref)
            case None      => true
          }
        }
        .map { case (key, importers) =>
          Import(List(Importer(key.ref, importers.flatMap(_.importees))))
        }
  }
  object OptimizationState extends KnownTypes {
    def initial(extraUsedNames: Set[String]): OptimizationState =
      OptimizationState(
        usedNames = predefinedTypeNames union extraUsedNames,
        keepFullyQualified = Set.empty,
        imports = Nil
      )
  }

  class WrappedRef(val ref: Term.Ref) {
    override def equals(obj: Any): Boolean =
      obj match {
        case wr: WrappedRef => ref.isEqual(wr.ref)
        case _              => false
      }

    override def hashCode(): Int = ref.toString().hashCode

    override def toString: String = ref.toString()
  }

  def make(
      scalafmt: Scalafmt,
      globalContext: GeneratorContext,
      target: CodeFileGeneratorContext.Target
  ): UIO[CodeFileGenerator] =
    for {
      contextRef <- Ref.make(CodeFileGeneratorContext(globalContext, target, Set.empty))
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

  def knownLocalName(name: String): ZIO[Has[CodeFileGenerator], Nothing, Unit] =
    ZIO.serviceWith(_.knownLocalName(name))
}
