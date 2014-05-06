package eu.inn.binders.naming

trait IdentifierParser {
  def parse(identifier: String, builder: IdentifierBuilder)
}
