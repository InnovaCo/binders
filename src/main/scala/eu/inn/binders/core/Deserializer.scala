package eu.inn.binders.core

import eu.inn.binders.naming.Converter

trait Deserializer[C <: Converter] {
  type nameConverterType = C
  def fieldName: Option[String]
  def isNull: Boolean
}
