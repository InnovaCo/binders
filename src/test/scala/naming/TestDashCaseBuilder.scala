package naming

import eu.inn.binders.naming.{DashCaseBuilder, PascalCaseParser, SnakeCaseBuilder}
import org.scalatest.{FlatSpec, Matchers}

class TestDashCaseBuilder extends FlatSpec with Matchers {
  "DashCaseBuilder " should " build string-like-this" in {

    val parser = new PascalCaseParser()
    val builder = new DashCaseBuilder()

    parser.parse("StringLikeThis", builder)
    val result = builder.toString

    assert(result == "string-like-this")
  }
}