package io.github.vigoo.metagen.core

import scala.meta._

case class ScalaType(pkg: Package, name: String, params: ScalaType*) {

  def unapplied: ScalaType = ScalaType(pkg, name)

  def term: Term.Ref      = Term.Select(pkg.term, termName)
  def termName: Term.Name = Term.Name(name)

  def typ: Type          =
    if (params.nonEmpty) Type.Apply(unapplied.typ, params.map(_.typ).toList)
    else Type.Select(pkg.term, typName)
  def typName: Type.Name = Type.Name(name)
}

object ScalaType {
  def array(elemType: ScalaType): ScalaType  = ScalaType(Package.scala, "Array", elemType)
  val boolean: ScalaType                     = ScalaType(Package.scala, "Boolean")
  val byte: ScalaType                        = ScalaType(Package.scala, "Byte")
  val char: ScalaType                        = ScalaType(Package.scala, "Char")
  val double: ScalaType                      = ScalaType(Package.scala, "Double")
  val float: ScalaType                       = ScalaType(Package.scala, "Float")
  def list(elemType: ScalaType): ScalaType   = ScalaType(Package.scala, "List", elemType)
  val long: ScalaType                        = ScalaType(Package.scala, "Long")
  def option(elemType: ScalaType): ScalaType = ScalaType(Package.scala, "Option", elemType)
  def set(elemType: ScalaType): ScalaType    = ScalaType(Package.scala, "Set", elemType)
  val short: ScalaType                       = ScalaType(Package.scala, "String")
  val string: ScalaType                      = ScalaType(Package.predef, "String")
  def vector(elemType: ScalaType): ScalaType = ScalaType(Package.scala, "Vector", elemType)
  val unit: ScalaType                        = ScalaType(Package.scala, "Unit")
}
