package eu.inn.binders.core

import eu.inn.binders.naming.Converter

trait Deserializer[C <: Converter] {
  type nameConverterType = C
  def hasField(fieldName: String): Boolean
  def iterator(): Iterator[Deserializer[C]]
}
