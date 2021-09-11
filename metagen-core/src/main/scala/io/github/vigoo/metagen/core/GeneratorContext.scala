package io.github.vigoo.metagen.core

import zio.nio.core.file.Path

case class GeneratorContext(scalaVersion: String, formattingEnabled: Boolean, root: Path)
