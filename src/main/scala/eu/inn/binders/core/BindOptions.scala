package eu.inn.binders.core

case class BindOptions(skipOptionalFields: Boolean)

object BindOptions {
  implicit val defaultBindOptions = new BindOptions(true)
  def get(implicit ops: BindOptions): BindOptions = ops
}
