package io.github.vigoo.metagen.core

import zio.nio.file.Path
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

  def asPath: Path = {
    val rev = path.reverse
    Path(rev.head, rev.tail: _*)
  }

  def /(child: String): Package = Package(NonEmptyList.Cons(child, path))

  def parent: Package =
    path.tail match {
      case Nil          => Package("_root_")
      case l @ ::(_, _) => Package(NonEmptyList.fromCons(l))
    }
}

object Package {
  def apply(first: String, rest: String*): Package = Package(NonEmptyList(first, rest: _*).reverse)

  val scala: Package    = Package("scala")
  val predef: Package   = Package("scala", "Predef")
  val javaUtil: Package = Package("java", "util")
  val javaLang: Package = Package("java", "lang")
  val javaMath: Package = Package("java", "math")
  val javaTime: Package = Package("java", "time")
}
