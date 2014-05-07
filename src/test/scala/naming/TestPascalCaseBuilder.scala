package naming

import eu.inn.binders.naming.{PascalCaseBuilder, SnakeCaseParser, CamelCaseBuilder}
import org.scalatest.{FlatSpec,Matchers}

class TestPascalCaseBuilder extends FlatSpec with Matchers {
  "PascalCaseBuilder " should " build stringLikeThis" in {

    val parser = new SnakeCaseParser()
    val builder = new PascalCaseBuilder()

    parser.parse("string_like_this", builder)
    val result = builder.toString

    assert(result == "StringLikeThis")
  }
}