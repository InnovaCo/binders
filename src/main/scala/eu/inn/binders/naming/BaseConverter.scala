package eu.inn.binders.naming

abstract class BaseConverter(val parser: IdentifierParser) extends Converter {
  def convert(identifier: String): String = {
    val b = createBuilder()
    parser.parse(identifier, b)
    b.toString()
  }
  def createBuilder(): IdentifierBuilder
}
