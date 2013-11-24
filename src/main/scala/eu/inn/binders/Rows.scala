package eu.inn.binders

trait Rows[R] {
  def iterator : Iterator[R]
}
