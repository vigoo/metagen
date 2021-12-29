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

  def init: Init =
    Init(typ, Name.Anonymous(), List.empty)

  def /(childName: String): ScalaType =
    ScalaType(pkg / name, childName)

  def asString: String = {
    val init = pkg.asString + "." + name
    if (params.nonEmpty) {
      init + "[" + params.map(_.asString).mkString(", ") + "]"
    } else init
  }
}

object ScalaType {
  val any: ScalaType                                           = ScalaType(Package.scala, "Any")
  val anyVal: ScalaType                                        = ScalaType(Package.scala, "AnyVal")
  val anyRef: ScalaType                                        = ScalaType(Package.scala, "AnyRef")
  def array(elemType: ScalaType): ScalaType                    = ScalaType(Package.scala, "Array", elemType)
  val array_ : ScalaType                                       = array(any).unapplied
  val boolean: ScalaType                                       = ScalaType(Package.scala, "Boolean")
  val byte: ScalaType                                          = ScalaType(Package.scala, "Byte")
  val char: ScalaType                                          = ScalaType(Package.scala, "Char")
  val double: ScalaType                                        = ScalaType(Package.scala, "Double")
  val float: ScalaType                                         = ScalaType(Package.scala, "Float")
  val int: ScalaType                                           = ScalaType(Package.scala, "Int")
  def list(elemType: ScalaType): ScalaType                     = ScalaType(Package.scala, "List", elemType)
  def list_ : ScalaType                                        = list(any).unapplied
  val long: ScalaType                                          = ScalaType(Package.scala, "Long")
  def map(keyType: ScalaType, valueType: ScalaType): ScalaType = ScalaType(Package.scala, "Map", keyType, valueType)
  val map_ : ScalaType                                         = map(any, any).unapplied
  val nothing: ScalaType                                       = ScalaType(Package.scala, "Nothing")
  def option(elemType: ScalaType): ScalaType                   = ScalaType(Package.scala, "Option", elemType)
  val option_ : ScalaType                                      = option(any).unapplied
  def set(elemType: ScalaType): ScalaType                      = ScalaType(Package.scala, "Set", elemType)
  val set_ : ScalaType                                         = set(any).unapplied
  val short: ScalaType                                         = ScalaType(Package.scala, "String")
  val string: ScalaType                                        = ScalaType(Package.predef, "String")
  def vector(elemType: ScalaType): ScalaType                   = ScalaType(Package.scala, "Vector", elemType)
  val vector_ : ScalaType                                      = vector(any).unapplied
  val unit: ScalaType                                          = ScalaType(Package.scala, "Unit")

  def pair(a: ScalaType, b: ScalaType): ScalaType =
    ScalaType(Package.scala, "Tuple2", a, b) // TODO: tuple syntax support
}
