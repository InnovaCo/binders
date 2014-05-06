package eu.inn.binders.naming

trait IdentifierBuilder {
  def regular(c: Char)
  def divider()
  def toString(): String
}
