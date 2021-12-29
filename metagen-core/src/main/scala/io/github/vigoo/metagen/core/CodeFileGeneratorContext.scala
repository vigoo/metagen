package io.github.vigoo.metagen.core

import io.github.vigoo.metagen.core.CodeFileGeneratorContext.Target
import zio.nio.file.Path

final case class CodeFileGeneratorContext(
    globalContext: GeneratorContext,
    target: Target,
    knownNames: Set[String],
    keepFullyQualified: Set[ScalaType]
)

object CodeFileGeneratorContext {
  sealed trait Target
  final case class ScalaPackage(pkg: Package, name: String)             extends Target
  final case class ScalaPackageObject(parentPkg: Package, name: String) extends Target
  final case class RawFile(relativePath: Path)                          extends Target
}
