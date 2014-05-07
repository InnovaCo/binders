package eu.inn.binders.naming

class CamelCaseParser extends IdentifierParser {
  override def parse(identifier: String, builder: IdentifierBuilder): Unit = {
    var prevIsSmallCaps = false
    for (c <- identifier) {
      if (c.isUpper) {
        if (prevIsSmallCaps) {
          builder.divider()
          builder.regular(c)
        }
        else {
          builder.regular(c)
        }
        prevIsSmallCaps = false
      }
      else {
        prevIsSmallCaps = true
        builder.regular(c)
      }
    }
  }
}
