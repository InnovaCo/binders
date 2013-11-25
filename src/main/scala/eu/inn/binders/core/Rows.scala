package eu.inn.binders.core

import scala.reflect.runtime.universe._

trait Rows[R] {
  type rowType = R
  def iterator : Iterator[R]
}
