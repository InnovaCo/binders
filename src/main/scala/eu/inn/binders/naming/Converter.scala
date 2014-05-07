package eu.inn.binders.naming

trait Converter {
  def convert(identifier: String): String
}
