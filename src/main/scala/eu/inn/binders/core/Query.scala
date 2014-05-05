package eu.inn.binders.core

trait Query[RS,S] {
  type rowsType = RS
  type statementType = S
	def createStatement(): S
  def executeStatement(statement: S) : RS
}
