package naming

import eu.inn.binders.naming.{SnakeCaseBuilder, CamelCaseParser}
import org.scalatest.{FlatSpec, Matchers}

class TestCamelCaseParser extends FlatSpec with Matchers {
  "CamelCaseParser " should " parse stringLikeThis " in {
    val parser = new CamelCaseParser()
    val builder = new SnakeCaseBuilder()

    parser.parse("stringLikeThis", builder)
    val result = builder.toString

    assert(result == "string_like_this")
  }

  "CamelCaseParser " should " parse string1Like2This3 " in {
    val parser = new CamelCaseParser()
    val builder = new SnakeCaseBuilder()

    parser.parse("string1Like2This3", builder)
    val result = builder.toString

    assert(result == "string1_like2_this3")
  }
}