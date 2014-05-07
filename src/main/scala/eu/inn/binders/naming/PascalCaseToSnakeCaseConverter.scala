package eu.inn.binders.naming

class PascalCaseToSnakeCaseConverter extends BaseConverter(new PascalCaseParser) {
  def createBuilder(): IdentifierBuilder = new SnakeCaseBuilder()
}
