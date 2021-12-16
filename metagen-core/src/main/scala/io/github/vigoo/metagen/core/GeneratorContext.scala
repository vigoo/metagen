package io.github.vigoo.metagen.core

import zio.nio.file.Path

case class GeneratorContext(scalaVersion: String, formattingEnabled: Boolean, root: Path)
