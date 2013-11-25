package eu.inn.binders

trait Query[RS,S] {
  type rowsType = RS
  type statementType = S
  def bindAndExecute(f : S => Unit) : RS
}
