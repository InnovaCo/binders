

import eu.inn.binders.core.FieldNotFoundException
import eu.inn.binders.dynamic.{Number, DynamicValue}
import org.scalatest._
import eu.inn.binders.dynamic._

case class TestDynamic(a:Int,b:String,c:Boolean)

class TestDynamicSpec extends FlatSpec with Matchers {
  "toDynamic " should " serialize int " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456
    val d1 = i1.toDynamic
    assert (d1.asInt == 123456)
  }

  "fromDynamic " should " deserialize int " in {
    import eu.inn.binders.dynamic._
    val d1 = Number(123456)
    val i1 = d1.fromDynamic[Int]
    assert (i1 == 123456)
  }

  "toDynamic " should " serialize long " in {
    import eu.inn.binders.dynamic._
    val i1 = 123456l
    val d1 = i1.toDynamic
    assert (d1.asLong == 123456l)
  }

  "fromDynamic " should " deserialize long " in {
    import eu.inn.binders.dynamic._
    val d1 = Number(Long.MaxValue)
    val i1 = d1.fromDynamic[Long]
    assert (i1 == Long.MaxValue)
  }

  "toDynamic " should " serialize string " in {
    import eu.inn.binders.dynamic._
    val i1 = "yey"
    val d1 = i1.toDynamic
    assert (d1.asString == "yey")
  }

  "fromDynamic " should " deserialize string " in {
    import eu.inn.binders.dynamic._
    val d1 = Text("ho")
    val i1 = d1.fromDynamic[String]
    assert (i1 == "ho")
  }

  "toDynamic " should " serialize Seq[Int] " in {
    import eu.inn.binders.dynamic._
    val i1 = Seq(1,2,3)
    val d1 = i1.toDynamic
    d1.asSeq should equal (Seq(1,2,3).map(Number(_)))
  }

  "fromDynamic " should " deserialize Seq[Int] " in {
    import eu.inn.binders.dynamic._
    val d1 = Lst(Seq(Number(1),Text("2"),Number(3)))
    val i1 = d1.fromDynamic[Seq[Int]]
    i1 should equal (Seq(1,2,3))
  }

  "toDynamic " should " serialize Map[String,Int] " in {
    import eu.inn.binders.dynamic._
    val i1: Map[String,Int] = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = i1.toDynamic
    d1.asMap should equal (i1 map toDynamicNumber)
  }

  "fromDynamic " should " deserialize Map[String,Int] " in {
    import eu.inn.binders.dynamic._
    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = Obj(m map toDynamicNumber)
    val i1 = d1.fromDynamic[Map[String,Int]]
    i1 should equal (m)
  }

  "toDynamic " should " serialize Obj " in {
    import eu.inn.binders.dynamic._
    val i1 = TestDynamic(1,"ho",true)
    val d1 = i1.toDynamic
    d1.asMap should equal (Map("a" -> Number(1), "b" -> Text("ho"), "c" -> Bool(true)))
  }

  "fromDynamic " should " deserialize StringMap = Map[String,String] " in {
    import eu.inn.binders.dynamic._
    import DefineType._

    val m = Obj(Map("a" -> Text("he"), "b" -> Text("ho")))
    val map = m.fromDynamic[StringMap]
    map should equal(Map("a"->"he", "b"->"ho"))
  }

  "DynamicValue " should " allow selectDynamic " in {
    val d = Obj(Map("a" -> Number(1), "b" -> Text("ho"), "c" -> Bool(true), "_" -> Bool(false)))
    val a = d.a[Int]
    a should equal(1)

    val b = d.b[String]
    b should equal("ho")

    val bo = d.b[Option[String]]
    bo should equal(Some("ho"))

    val z = d.zz[Option[String]]
    z should equal(None)

    val y = d.y[Option[Int]]
    y should equal(None)

    val f = d.__[Boolean]
    f should equal(false)

    intercept[FieldNotFoundException] {
      d.zeo[String]
    }
  }

  def toDynamicNumber(kv: (String, Int)) = {
    (kv._1, Number(kv._2))
  }
}
