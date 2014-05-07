package naming

import eu.inn.binders.naming.{SnakeCaseBuilder, PascalCaseParser, SnakeCaseParser}
import org.scalatest.{FlatSpec,Matchers}

class TestPascalCaseParser extends FlatSpec with Matchers {
  "PascalCaseParser " should " parse StringLikeThis " in {

    val parser = new PascalCaseParser()
    val builder = new SnakeCaseBuilder()

    parser.parse("StringLikeThis", builder)
    val result = builder.toString

    assert(result == "string_like_this")
  }
}