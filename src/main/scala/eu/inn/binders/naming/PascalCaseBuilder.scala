package eu.inn.binders.naming

class PascalCaseBuilder(possibleLength: Option[Int] = None) extends CamelCaseBuilder(possibleLength) {
  nextIsUpperCase = true
}
