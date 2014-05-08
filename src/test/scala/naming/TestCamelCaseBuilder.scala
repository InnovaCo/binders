package naming

import eu.inn.binders.naming.{CamelCaseBuilder, SnakeCaseParser}
import org.scalatest.{FlatSpec, Matchers}

class TestCamelCaseBuilder extends FlatSpec with Matchers {
  "CamelCaseBuilder " should " build stringLikeThis" in {

    val parser = new SnakeCaseParser()
    val builder = new CamelCaseBuilder()

    parser.parse("string_like_this", builder)
    val result = builder.toString

    assert(result == "stringLikeThis")
  }
}