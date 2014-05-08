package naming

import eu.inn.binders.naming.{SnakeUpperCaseBuilder, PascalCaseParser}
import org.scalatest.{FlatSpec, Matchers}

class TestSnakeUpperCaseBuilder extends FlatSpec with Matchers {
  "SnakeCaseBuilder " should " build STRING_LIKE_THIS" in {

    val parser = new PascalCaseParser()
    val builder = new SnakeUpperCaseBuilder()

    parser.parse("StringLikeThis", builder)
    val result = builder.toString

    assert(result == "STRING_LIKE_THIS")
  }
}