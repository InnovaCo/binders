package eu.inn.binders.naming

class UppercaseConverter extends Converter {
  def convert(identifier: String): String = identifier.toUpperCase
}
