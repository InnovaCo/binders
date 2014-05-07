package eu.inn.binders.naming

class CamelCaseToSnakeCaseConverter extends BaseConverter(new CamelCaseParser) {
  def createBuilder(): IdentifierBuilder = new SnakeCaseBuilder()
}
