package eu.inn.binders.core

trait Query[S] {
  type statementType = S

  def createStatement(): S
}
