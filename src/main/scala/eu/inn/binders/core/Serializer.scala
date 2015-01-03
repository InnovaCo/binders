package eu.inn.binders.core

import eu.inn.binders.naming.Converter

trait Serializer[C <: Converter] {
  type nameConverterType = C
  // def hasField(fieldName: String): Boolean
  // def getFieldSerializer(fieldName: String): Option[Serializer[C]]
}
