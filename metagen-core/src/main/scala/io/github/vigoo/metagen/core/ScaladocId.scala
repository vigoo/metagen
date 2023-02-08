package io.github.vigoo.metagen.core

import java.util.UUID
import scala.meta.Lit

final case class ScaladocId(value: UUID) {
  def lit: Lit.String = Lit.String(value.toString)
}

object ScaladocId {
  def next: ScaladocId = ScaladocId(UUID.randomUUID())
}
