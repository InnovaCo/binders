package eu.inn.binders.core


trait Rows[R] {
  type rowType = R

  def iterator(): Iterator[R]
}
