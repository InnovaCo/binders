package eu.inn.binders.naming

class LowercaseConverter extends Converter {
  def convert(identifier: String): String = identifier.toLowerCase
}
