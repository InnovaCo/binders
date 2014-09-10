package eu.inn.binders.core

trait Statement[RS] {
  type rowsType = RS

  def hasParameter(parameterName: String): Boolean

  def execute(): RS
}
