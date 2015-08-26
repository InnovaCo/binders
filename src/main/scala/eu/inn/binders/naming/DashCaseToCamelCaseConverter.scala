package eu.inn.binders.naming

class DashCaseToCamelCaseConverter extends BaseConverter(new DashCaseParser) {
  def createBuilder(): IdentifierBuilder = new CamelCaseBuilder()
}
