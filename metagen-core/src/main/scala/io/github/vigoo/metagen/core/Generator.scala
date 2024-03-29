package io.github.vigoo.metagen.core

import org.scalafmt.interfaces.Scalafmt
import zio.nio.file.{Files, Path}
import zio.{IO, Ref, UIO, ZEnvironment, ZIO, ZLayer}

import scala.meta.Term

trait Generator {
  def generateScalaPackage[R, E](pkg: Package, name: String)(
      contents: ZIO[R with CodeFileGenerator, E, Term.Block]
  ): ZIO[R, GeneratorFailure[E], Path] =
    generateScalaTarget[R, E](CodeFileGeneratorContext.ScalaPackage(pkg, name))(contents)
  def generateScalaPackageObject[R, E](pkg: Package, name: String)(
      contents: ZIO[R with CodeFileGenerator, E, Term.Block]
  ): ZIO[R, GeneratorFailure[E], Path] =
    generateScalaTarget[R, E](CodeFileGeneratorContext.ScalaPackageObject(pkg, name))(contents)

  def generateRawFile[R, E](relativePath: Path)(
      contents: ZIO[R with CodeFileGenerator, E, String]
  ): ZIO[R, GeneratorFailure[E], Path]
  def generateScalaTarget[R, E](target: CodeFileGeneratorContext.Target)(
      contents: ZIO[R with CodeFileGenerator, E, Term.Block]
  ): ZIO[R, GeneratorFailure[E], Path]

  def setScalaVersion(version: String): UIO[Unit]
  def enableFormatting(): UIO[Unit]
  def disableFormatting(): UIO[Unit]
  def setRoot(root: Path): UIO[Unit]
}

object Generator {
  class Live(contextRef: Ref[GeneratorContext], scalafmt: Scalafmt) extends Generator {
    override def generateScalaTarget[R, E](target: CodeFileGeneratorContext.Target)(
        contents: ZIO[R with CodeFileGenerator, E, Term.Block]
    ): ZIO[R, GeneratorFailure[E], Path] =
      for {
        context           <- contextRef.get
        codeFileGenerator <- CodeFileGenerator.make(scalafmt, context, target)
        generator          =
          for {
            tree          <- contents.mapError(GeneratorFailure.CustomFailure.apply)
            optimizedTree <- CodeFileGenerator.seal(tree)
            path          <- CodeFileGenerator.targetPath
            unformatted   <- CodeFileGenerator.prettyPrint(optimizedTree)
            withDoc       <- CodeFileGenerator.injectScaladocToGeneratedCode(unformatted)
            _             <- path.parent match {
                               case Some(directory) =>
                                 Files.createDirectories(directory).mapError(GeneratorFailure.FailedToCreateDirectories)
                               case None            => ZIO.unit
                             }
            _             <- if (context.formattingEnabled) {
                               for {
                                 formatted <- CodeFileGenerator.format(withDoc)
                                 _         <- CodeFileGenerator.writeIfDifferent(formatted)
                               } yield ()
                             } else {
                               CodeFileGenerator.writeIfDifferent(withDoc)
                             }
          } yield path
        env               <- ZIO.environment[R]
        fileEnv            = env ++ ZEnvironment(codeFileGenerator)
        path              <- generator.provideEnvironment(fileEnv)
      } yield path

    def generateRawFile[R, E](relativePath: Path)(
        contents: ZIO[R with CodeFileGenerator, E, String]
    ): ZIO[R, GeneratorFailure[E], Path] =
      for {
        context           <- contextRef.get
        codeFileGenerator <- CodeFileGenerator.make(scalafmt, context, CodeFileGeneratorContext.RawFile(relativePath))
        generator          = for {
                               path <- CodeFileGenerator.targetPath
                               raw  <- contents.mapError(GeneratorFailure.CustomFailure.apply)
                               _    <- path.parent match {
                                         case Some(directory) =>
                                           Files.createDirectories(directory).mapError(GeneratorFailure.FailedToCreateDirectories)
                                         case None            => ZIO.unit
                                       }
                               _    <- CodeFileGenerator.writeIfDifferent(raw)
                             } yield path
        env               <- ZIO.environment[R]
        fileEnv            = env ++ ZEnvironment(codeFileGenerator)
        path              <- generator.provideEnvironment(fileEnv)
      } yield path

    override def setScalaVersion(version: String): UIO[Unit] =
      contextRef.update(_.copy(scalaVersion = version))

    override def enableFormatting(): UIO[Unit] =
      contextRef.update(_.copy(formattingEnabled = true))

    override def disableFormatting(): UIO[Unit] =
      contextRef.update(_.copy(formattingEnabled = false))

    override def setRoot(root: Path): UIO[Unit] =
      contextRef.update(_.copy(root = root))
  }

  val defaultScalaVersion: String = "2.13.6"

  def make: IO[GeneratorFailure[Nothing], Generator] =
    for {
      scalafmt   <- ZIO.attempt(Scalafmt.create(this.getClass.getClassLoader)).mapError(GeneratorFailure.ScalaFmtFailure)
      contextRef <- Ref.make(GeneratorContext(defaultScalaVersion, formattingEnabled = true, root = Path(".")))
    } yield new Live(contextRef, scalafmt)

  val live: ZLayer[Any, GeneratorFailure[Nothing], Generator] = ZLayer(make)

  def generateScalaPackage[R, E](pkg: Package, name: String)(
      contents: ZIO[R with CodeFileGenerator, E, Term.Block]
  ): ZIO[R with Generator, GeneratorFailure[E], Path] =
    ZIO.service[Generator].flatMap(_.generateScalaPackage[R, E](pkg, name)(contents))

  def generateScalaPackageObject[R, E](pkg: Package, name: String)(
      contents: ZIO[R with CodeFileGenerator, E, Term.Block]
  ): ZIO[R with Generator, GeneratorFailure[E], Path] =
    ZIO.service[Generator].flatMap(_.generateScalaPackageObject[R, E](pkg, name)(contents))

  def generateRawFile[R, E](relativePath: Path)(
      contents: ZIO[R with CodeFileGenerator, E, String]
  ): ZIO[R with Generator, GeneratorFailure[E], Path] =
    ZIO.service[Generator].flatMap(_.generateRawFile[R, E](relativePath)(contents))

  def setScalaVersion(version: String): ZIO[Generator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.setScalaVersion(version))

  def enableFormatting(): ZIO[Generator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.enableFormatting())

  def disableFormatting(): ZIO[Generator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.disableFormatting())

  def setRoot(path: Path): ZIO[Generator, Nothing, Unit] =
    ZIO.serviceWithZIO(_.setRoot(path))
}
