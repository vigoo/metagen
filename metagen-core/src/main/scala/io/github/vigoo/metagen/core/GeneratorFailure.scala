package io.github.vigoo.metagen.core

import java.io.IOException

sealed trait GeneratorFailure[+E]
object GeneratorFailure {
  final case class CustomFailure[+E](error: E)                    extends GeneratorFailure[E]
  final case class PrettyPrintingFailure(reason: Throwable)       extends GeneratorFailure[Nothing]
  final case class FailedToReadFile(reason: IOException)          extends GeneratorFailure[Nothing]
  final case class FailedToWriteFile(reason: IOException)         extends GeneratorFailure[Nothing]
  final case class FailedToCreateDirectories(reason: IOException) extends GeneratorFailure[Nothing]
  final case class ScalaFmtFailure(reason: Throwable)             extends GeneratorFailure[Nothing]
  final case class TreeTransformationFailure(reason: Throwable)   extends GeneratorFailure[Nothing]
}
