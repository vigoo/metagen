package io.github.vigoo.metagen.core

import org.scalafmt.interfaces.Scalafmt
import zio.blocking.Blocking
import zio.nio.core.file.Path
import zio.nio.file.Files
import zio.{Has, Ref, Task, UIO, ZIO, ZLayer}

import scala.meta.Term

trait Generator {
  def generateFile[E](pkg: Package, name: String)(
      contents: ZIO[Has[CodeFileGenerator], E, Term.Block]
  ): ZIO[Blocking, GeneratorFailure[E], Path]
  def setScalaVersion(version: String): UIO[Unit]
  def enableFormatting(): UIO[Unit]
  def disableFormatting(): UIO[Unit]
  def setRoot(root: Path): UIO[Unit]
}

object Generator {
  class Live(contextRef: Ref[GeneratorContext], scalafmt: Scalafmt) extends Generator {
    override def generateFile[E](pkg: Package, name: String)(
        contents: ZIO[Has[CodeFileGenerator], E, Term.Block]
    ): ZIO[Blocking, GeneratorFailure[E], Path] =
      for {
        context           <- contextRef.get
        codeFileGenerator <- CodeFileGenerator.make(scalafmt, context, pkg, name)
        generator          =
          for {
            tree          <- contents.mapError(GeneratorFailure.CustomFailure.apply)
            optimizedTree <- CodeFileGenerator.seal(tree)
            path          <- CodeFileGenerator.targetPath
            unformatted   <- CodeFileGenerator.prettyPrint(optimizedTree)
            _             <- path.parent match {
                               case Some(directory) =>
                                 Files.createDirectories(directory).mapError(GeneratorFailure.FailedToCreateDirectories)
                               case None            => ZIO.unit
                             }
            _             <- if (context.formattingEnabled) {
                               for {
                                 formatted <- CodeFileGenerator.format(unformatted)
                                 _         <- CodeFileGenerator.writeIfDifferent(formatted)
                               } yield ()
                             } else {
                               CodeFileGenerator.writeIfDifferent(unformatted)
                             }
          } yield path
        path              <- generator.provideSome[Blocking](_ ++ Has(codeFileGenerator))
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

  def make: Task[Generator] =
    for {
      scalafmt   <- ZIO.effect(Scalafmt.create(this.getClass.getClassLoader))
      contextRef <- Ref.make(GeneratorContext(defaultScalaVersion, formattingEnabled = true, root = Path(".")))
    } yield new Live(contextRef, scalafmt)

  val live: ZLayer[Any, Throwable, Has[Generator]] = make.toLayer

  def generateFile[E](pkg: Package, name: String)(
      contents: ZIO[Has[CodeFileGenerator], E, Term.Block]
  ): ZIO[Blocking with Has[Generator], GeneratorFailure[E], Path] =
    ZIO.service[Generator].flatMap(_.generateFile(pkg, name)(contents))

  def setScalaVersion(version: String): ZIO[Has[Generator], Nothing, Unit] =
    ZIO.serviceWith(_.setScalaVersion(version))

  def enableFormatting(): ZIO[Has[Generator], Nothing, Unit] =
    ZIO.serviceWith(_.enableFormatting())

  def disableFormatting(): ZIO[Has[Generator], Nothing, Unit] =
    ZIO.serviceWith(_.disableFormatting())

  def setRoot(path: Path): ZIO[Has[Generator], Nothing, Unit] =
    ZIO.serviceWith(_.setRoot(path))
}
