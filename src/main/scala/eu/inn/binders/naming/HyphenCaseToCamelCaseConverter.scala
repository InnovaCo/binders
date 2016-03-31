package eu.inn.binders.naming

class HyphenCaseToCamelCaseConverter extends BaseConverter(new DashCaseParser) {
  def createBuilder(): IdentifierBuilder = new CamelCaseBuilder()
}



