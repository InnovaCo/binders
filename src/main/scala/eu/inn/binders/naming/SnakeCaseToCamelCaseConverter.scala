package eu.inn.binders.naming

class SnakeCaseToCamelCaseConverter extends BaseConverter(new SnakeCaseParser) {
  def createBuilder(): IdentifierBuilder = new CamelCaseBuilder()
}
