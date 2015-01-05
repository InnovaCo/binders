package eu.inn.binders.core

import eu.inn.binders.naming.Converter

trait Deserializer[C <: Converter] {
  type nameConverterType = C
  // def hasField(fieldName: String): Boolean
  // def iterator[R <: Deserializer[C]](): Iterator[R] -- can't override, but could be defined

  def fieldName: Option[String]
}
