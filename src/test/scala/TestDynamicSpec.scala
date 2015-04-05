
import eu.inn.binders.dynamic.DynamicObject
import org.scalatest._

class TestDynamicSpec extends FlatSpec with Matchers {
  /*"toDynamic " should " match int " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456
    val d1 = i1.toDynamic
    assert (d1.asInt == 123456)
  }*/

  "toDynamic " should " serialize int " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456
    val d1 = i1.toDynamic
    assert (d1.asInt == 123456)
  }

  "toDynamic " should " serialize long " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456l
    val d1 = i1.toDynamic
    assert (d1.asLong == 123456l)
  }

  "toDynamic " should " serialize string " in {
    import eu.inn.binders.dynamic._
    val i1 = "yey"
    val d1 = i1.toDynamic
    assert (d1.asString == "yey")
  }

  "toDynamic " should " serialize Seq[Int] " in {
    import eu.inn.binders.dynamic._
    val i1 = Seq(1,2,3)
    val d1 = i1.toDynamic
    d1.asSeq should contain allOf (DynamicObject(1),DynamicObject(2))// i1//(List(1,2,3).map(DynamicObject(_)))
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
