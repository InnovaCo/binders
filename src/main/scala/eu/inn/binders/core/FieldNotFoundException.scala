package eu.inn.binders.core

class FieldNotFoundException(val fieldName: String) extends RuntimeException(s"Field $fieldName wasn't found")
