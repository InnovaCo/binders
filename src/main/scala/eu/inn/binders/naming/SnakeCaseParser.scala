package eu.inn.binders.naming

class SnakeCaseParser extends IdentifierParser {
  override def parse(identifier: String, builder: IdentifierBuilder): Unit = {
    var prevIsUnderscore = false
    var underscoreConsumed = false
    for (c <- identifier) {
      if (prevIsUnderscore) {
        builder.divider()
        builder.regular(c)
        underscoreConsumed = true
      }
      else {
        if (c == '_') {
          prevIsUnderscore = true
          underscoreConsumed = false
        } else {
          builder.regular(c)
          prevIsUnderscore = false
        }
      }
    }
    if (prevIsUnderscore && !underscoreConsumed) {
      builder.regular('_')
    }
  }
}
