package eu.inn.binders.core

trait Statement {
  def hasParameter(parameterName: String) : Boolean
}
