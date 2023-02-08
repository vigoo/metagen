package io.github.vigoo.metagen.core

import org.scalafmt.interfaces.Scalafmt
import zio.nio.file.Path
import zio.nio.file.Files
import zio.{Chunk, IO, Ref, UIO, ZIO}

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.meta._
import scala.meta.contrib.XtensionTreeEquality
import scala.meta.internal.prettyprinters.TreeSyntax

trait CodeFileGenerator {
  def targetPath: UIO[Path]
  def writeIfDifferent(contents: String): ZIO[Any, GeneratorFailure[Nothing], Unit]
  def prettyPrint(tree: Tree): IO[GeneratorFailure[Nothing], String]
  def format(contents: String): ZIO[Any, GeneratorFailure[Nothing], String]
  def seal(tree: Term.Block): IO[GeneratorFailure[Nothing], Tree]
  def knownLocalName(name: String): IO[Nothing, Unit]
  def keepFullyQualified(typ: ScalaType): IO[Nothing, Unit]
  def addScaladoc(lines: String): IO[Nothing, Mod.Annot]
  def injectScaladocToGeneratedCode(code: String): IO[Nothing, String]
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

    def writeIfDifferent(contents: String): ZIO[Any, GeneratorFailure[Nothing], Unit] =
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
          .attempt {
            val prettyPrinter = TreeSyntax[Tree](dialect)
            prettyPrinter(tree).toString
          }
          .mapError(GeneratorFailure.PrettyPrintingFailure)
      }

    def format(contents: String): ZIO[Any, GeneratorFailure[Nothing], String] =
      contextRef.get.flatMap { context =>
        val version =
          if (context.globalContext.scalaVersion.startsWith("3.")) "3"
          else if (context.globalContext.scalaVersion.startsWith("2.13.")) "2.13"
          else "2.12"

        targetPath
          .flatMap { path =>
            ZIO.attemptBlocking {
              scalafmt.format(Paths.get(s".scalafmt.$version.conf"), path.toFile.toPath, contents)
            }
          }
          .mapError(GeneratorFailure.ScalaFmtFailure)
      }

    def knownLocalName(name: String): IO[Nothing, Unit] =
      contextRef.update { context =>
        context.copy(knownNames = context.knownNames + name)
      }

    def keepFullyQualified(typ: ScalaType): IO[Nothing, Unit] =
      contextRef.update { context =>
        context.copy(keepFullyQualified = context.keepFullyQualified + typ)
      }

    def seal(tree: Term.Block): ZIO[Any, GeneratorFailure[Nothing], Tree] =
      contextRef.get.flatMap { context =>
        ZIO
          .attempt {
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

            val definedNames =
              tree.collect {
                case Defn.Class(_, name, _, _, _) => name.value
                case Defn.Trait(_, name, _, _, _) => name.value
              }.toSet

            val localNames =
              tree.collect {
                case Pat.Var(name)                 => name.value
                case Defn.Def(_, name, _, _, _, _) => name.value
                case Term.Param(_, name, _, _)     => name.value
              }.toSet

            val initialState =
              OptimizationState.initial(definedNames union context.knownNames, context.keepFullyQualified)

            val finalState = usedTypes.foldLeft(initialState) { case (state, (ref, names)) =>
              val isFromPredef = ref.ref.isEqual(Package.scala.term) || ref.ref.isEqual(Package.predef.term)

              names.foldLeft(state) { case (state, name) =>
                val collidingPredef = state.collidingPredefs.contains(name)
                if (!isFromPredef || collidingPredef) {
                  if (state.usedNames.contains(name) || collidingPredef) {
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
                } else {
                  state
                }
              }
            }

            val transformedTree =
              tree
                .transform {
                  case t: Type.Select
                      if !isInImportStatement(t) &&
                        !finalState.keepFullyQualified.contains(t.toString()) &&
                        finalState.importsType(t) =>
                    // Dropping selector
                    t.name

                  case t: Type.Select =>
                    val needRootPrefix = firstTermOf(t.qual) match {
                      case Some(name) => localNames.contains(name)
                      case None       => false
                    }
                    if (needRootPrefix) {
                      prependSelector(t, Term.Name("_root_"))
                    } else {
                      t
                    }

                  case t: Term.Select =>
                    val needRootPrefix = firstTermOf(t.qual) match {
                      case Some(name) => localNames.contains(name)
                      case None       => false
                    }
                    t.qual match {
                      case tt: Term.Select
                          if !isInImportStatement(t) && !finalState.keepFullyQualified.contains(
                            tt.toString()
                          ) && finalState.importsTerm(tt) =>
                        if (needRootPrefix && finalState.keepFullyQualified.contains(tt.toString()))
                          prependSelector(Term.Select(tt.name, t.name), Term.Name("_root_"))
                        else
                          Term.Select(tt.name, t.name)
                      case _ =>
                        if (needRootPrefix && finalState.keepFullyQualified.contains(t.toString()))
                          prependSelector(t, Term.Name("_root_"))
                        else
                          t
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

    override def addScaladoc(lines: String): IO[Nothing, Mod.Annot] =
      contextRef.modify { context =>
        val id             = ScaladocId.next
        val updatedContext = context.copy(scaladocs = context.scaladocs + (id -> lines))
        (mod"@io.github.vigoo.metagen.core.generatedScaladoc(${id.lit})", updatedContext)
      }

    override def injectScaladocToGeneratedCode(code: String): IO[Nothing, String] =
      contextRef.get.map { context =>
        context.scaladocs
          .foldLeft(code) { case (code, (id, lines)) =>
            val splitLines       = lines.linesIterator.toVector
            val renderedScaladoc =
              if (splitLines.size == 1)
                "/** " + lines + " */ "
              else
                "/**\n" + lines.linesIterator.map(" * " + _).mkString("\n") + "\n */\n"
            val from             = "@io.github.vigoo.metagen.core.generatedScaladoc(\"" + id.value.toString + "\")"
            code.replace(from, renderedScaladoc)
          }
          .replace("""import io.github.vigoo.metagen.core.generatedScaladoc""", "")
      }

    @tailrec
    private def firstTermOf(term: Term): Option[String] =
      term match {
        case Term.Select(qual, _) => firstTermOf(qual)
        case Term.Name(name)      => Some(name)
        case _                    => None
      }

    private def prependSelector(selector: Type.Select, name: Term.Name): Type.Select =
      selector.qual match {
        case n: Term.Name   => Type.Select(Term.Select(name, n), selector.name)
        case s: Term.Select => Type.Select(prependSelector(s, name), selector.name)
        case _              => selector
      }

    private def prependSelector(selector: Term.Select, name: Term.Name): Term.Select = {
      val newQual = selector.qual match {
        case s: Term.Select => prependSelector(s, name)
        case n: Term.Name   => Term.Select(name, n)
      }
      Term.Select(newQual, selector.name)
    }

    @tailrec
    private def isInImportStatement(t: Tree): Boolean =
      t.parent match {
        case Some(Import(_)) => true
        case Some(parent)    => isInImportStatement(parent)
        case None            => false
      }
  }

  case class OptimizationState(
      usedNames: Set[String],
      keepFullyQualified: Set[String],
      imports: List[Import],
      collidingPredefs: Set[String]
  ) {
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
    def initial(extraUsedNames: Set[String], keepFullyQualified: Set[ScalaType]): OptimizationState =
      OptimizationState(
        usedNames = (predefinedTypeNames diff extraUsedNames) union (extraUsedNames diff predefinedTypeNames),
        keepFullyQualified = keepFullyQualified.map(_.asString),
        imports = Nil,
        collidingPredefs = predefinedTypeNames intersect extraUsedNames
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
      contextRef <- Ref.make(
                      CodeFileGeneratorContext(
                        globalContext,
                        target,
                        knownNames = Set.empty,
                        keepFullyQualified =
                          Set(ScalaType(Package("io", "github", "vigoo", "metagen", "core"), "generatedScaladoc")),
                        scaladocs = Map.empty
                      )
                    )
    } yield new Live(contextRef, scalafmt)

  def targetPath: ZIO[CodeFileGenerator, Nothing, Path] =
    ZIO.serviceWithZIO(_.targetPath)

  def writeIfDifferent(contents: String): ZIO[CodeFileGenerator, GeneratorFailure[Nothing], Unit] =
    ZIO.service[CodeFileGenerator].flatMap(_.writeIfDifferent(contents))

  def prettyPrint(tree: Tree): ZIO[CodeFileGenerator, GeneratorFailure[Nothing], String] =
    ZIO.serviceWithZIO(_.prettyPrint(tree))

  def format(contents: String): ZIO[CodeFileGenerator, GeneratorFailure[Nothing], String] =
    ZIO.service[CodeFileGenerator].flatMap(_.format(contents))

  def seal(tree: Term.Block): ZIO[CodeFileGenerator, GeneratorFailure[Nothing], Tree] =
    ZIO.serviceWithZIO(_.seal(tree))

  def knownLocalName(name: String): ZIO[CodeFileGenerator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.knownLocalName(name))

  def keepFullyQualified(typ: ScalaType): ZIO[CodeFileGenerator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.keepFullyQualified(typ))

  def addScaladoc(lines: String): ZIO[CodeFileGenerator, Nothing, Mod.Annot] =
    ZIO.serviceWithZIO(_.addScaladoc(lines))

  def injectScaladocToGeneratedCode(code: String): ZIO[CodeFileGenerator, Nothing, String] =
    ZIO.serviceWithZIO(_.injectScaladocToGeneratedCode(code))
}
