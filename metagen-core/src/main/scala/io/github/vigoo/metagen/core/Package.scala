package io.github.vigoo.metagen.core

import zio.nio.core.file.Path
import zio.prelude.NonEmptyList

import scala.meta._

case class Package(path: NonEmptyList[String]) {

  def term: Term.Ref =
    path match {
      case NonEmptyList.Cons(head, tail) => Term.Select(Package(tail).term, Term.Name(head))
      case NonEmptyList.Single(head)     => Term.Name(head)
    }

  def typ: Type =
    path match {
      case NonEmptyList.Cons(head, tail) => Type.Select(Package(tail).term, Type.Name(head))
      case NonEmptyList.Single(head)     => Type.Name(head)
    }

  def asPath: Path = Path(path.head, path.tail: _*)
}

object Package {
  val scala: Package  = Package(NonEmptyList("scala"))
  val predef: Package = Package(NonEmptyList("scala", "Predef"))
}
