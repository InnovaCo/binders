
import org.scalatest.{FlatSpec, Matchers}

class TestDynamicSpec extends FlatSpec with Matchers {
  "toDynamic " should " match int " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456
    val d1 = i1.toDynamic


    d1 match {
      case i: Int => println("i = " + i)
      case _ => println("d1 ~ " + d1)
    }
  }

  "toDynamic " should " serialize int " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456
    val d1 = i1.toDynamic
    assert (d1 == 123456)
  }

  "toDynamic " should " serialize long " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456l
    val d1 = i1.toDynamic
    assert (d1 == 123456l)
  }

  "toDynamic " should " serialize string " in {
    import eu.inn.binders.dynamic._
    val i1 = "yey"
    val d1 = i1.toDynamic
    assert (d1 == "yey")
  }

  "toDynamic " should " serialize Seq[Int] " in {
    import eu.inn.binders.dynamic._
    val i1 = Seq(1,2,3)
    val d1 = i1.toDynamic
    assert (d1 == Seq(1,2,3))
  }

  /*
  "toDynamic " should " serialize Map[String,Int] " in {
    import eu.inn.binders.dynamic._
    val i1 = Map("f1" -> 1, "f2" -> 2, "f3" -> 3)
    val d1 = i1.toDynamic
    assert (d1.asMap ==
      Map("f1" -> DynamicObject(1), "f2" -> DynamicObject(1), "f3" -> DynamicObject(1))
    )
  }
  */
}
