package eu.inn.binders

trait Statement {
  def hasParameter(parameterName: String) : Boolean
}
