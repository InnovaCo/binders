package eu.inn.binders.core

trait Row {
  def hasField(fieldName: String): Boolean
}
