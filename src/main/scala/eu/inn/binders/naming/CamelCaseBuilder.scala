package eu.inn.binders.naming

import scala.StringBuilder

class CamelCaseBuilder(possibleLength: Option[Int] = None) extends IdentifierBuilder {
  private val sb = possibleLength.map {
    new StringBuilder(_)
  } getOrElse {
    new StringBuilder
  }

  private var nextIsUpperCase = false

  override def divider(): Unit = {
    nextIsUpperCase = true
  }

  override def regular(c: Char): Unit = {
    if (nextIsUpperCase)
      sb.append(c.toUpper)
    else
      sb.append(c)
  }

  override def toString = {
    sb.toString
  }
}
