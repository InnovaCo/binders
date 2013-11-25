package eu.inn.binders.core

trait Query[RS,S] {
  type rowsType = RS
  type statementType = S
  def bindAndExecute(f : S => Unit) : RS
}
