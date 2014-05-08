package naming

import eu.inn.binders.naming.{CamelCaseBuilder, SnakeUpperCaseParser}
import org.scalatest.{FlatSpec, Matchers}

class TestSnakeUpperCaseParser extends FlatSpec with Matchers {
  "SnakeUpperCaseParser " should " parse STRING_LIKE_THIS " in {

    val parser = new SnakeUpperCaseParser()
    val builder = new CamelCaseBuilder()

    parser.parse("STRING_LIKE_THIS", builder)
    val result = builder.toString

    assert(result == "stringLikeThis")
  }
}