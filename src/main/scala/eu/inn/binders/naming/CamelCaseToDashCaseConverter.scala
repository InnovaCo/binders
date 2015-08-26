package eu.inn.binders.naming

class CamelCaseToDashCaseConverter extends BaseConverter(new CamelCaseParser) {
  def createBuilder(): IdentifierBuilder = new DashCaseBuilder()
}
