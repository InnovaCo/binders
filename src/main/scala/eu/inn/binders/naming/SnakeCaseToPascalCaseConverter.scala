package eu.inn.binders.naming

class SnakeCaseToPascalCaseConverter extends BaseConverter(new SnakeCaseParser) {
  def createBuilder(): IdentifierBuilder = new PascalCaseBuilder()
}
