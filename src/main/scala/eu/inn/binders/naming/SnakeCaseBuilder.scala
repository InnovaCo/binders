package eu.inn.binders.naming


class SnakeCaseBuilder(possibleLength: Option[Int] = None) extends IdentifierBuilder {
  private val sb = possibleLength.map {
    new StringBuilder(_)
  } getOrElse {
    new StringBuilder
  }

  private var prevIsDivider = false

  override def divider(): Unit = {
    sb.append('_')
    prevIsDivider = true
  }

  override def regular(c: Char): Unit = {
    if (prevIsDivider) {
      sb.append(c.toLower)
    }
    else {
      sb.append(c)
    }
    prevIsDivider = false
  }

  override def toString() = {
    sb.toString()
  }
}
