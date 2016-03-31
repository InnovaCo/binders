package eu.inn.binders.naming

class CamelCaseToHyphenCaseConverter extends BaseConverter(new CamelCaseParser) {
  def createBuilder(): IdentifierBuilder = new HyphenCaseBuilder()
}
