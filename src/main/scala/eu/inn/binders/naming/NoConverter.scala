package eu.inn.binders.naming

class NoConverter extends Converter {
  def convert(identifier: String): String = identifier
}
