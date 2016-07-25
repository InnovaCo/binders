

import eu.inn.binders.value.{Number, _}
import org.scalatest._

case class TestDynamic(a:Int,b:String,c:Boolean)

class TestDynamicSpec extends FlatSpec with Matchers {

  "toValue " should " serialize int " in {
    import eu.inn.binders.value._
    val i1 = 123456
    val d1 = i1.toValue
    assert (d1.asInt == 123456)
  }

  "fromValue " should " deserialize int " in {
    import eu.inn.binders.value._
    val d1 = Number(123456)
    val i1 = d1.fromValue[Int]
    assert (i1 == 123456)
  }

  "toValue " should " serialize long " in {
    import eu.inn.binders.value._
    val i1 = 123456l
    val d1 = i1.toValue
    assert (d1.asLong == 123456l)
  }

  "fromValue " should " deserialize long " in {
    import eu.inn.binders.value._
    val d1 = Number(Long.MaxValue)
    val i1 = d1.fromValue[Long]
    assert (i1 == Long.MaxValue)
  }

  "toValue " should " serialize string " in {
    import eu.inn.binders.value._
    val i1 = "yey"
    val d1 = i1.toValue
    assert (d1.asString == "yey")
  }

  "fromValue " should " deserialize string " in {
    import eu.inn.binders.value._
    val d1 = Text("ho")
    val i1 = d1.fromValue[String]
    assert (i1 == "ho")
  }

  "toValue " should " serialize null " in {
    import eu.inn.binders.value._
    val i1: String = null
    val d1 = i1.toValue
    assert (d1 == Null)
  }

  "fromValue " should " deserialize null " in {
    import eu.inn.binders.value._
    val d1 = Null
    val i1 = d1.fromValue[Option[String]]
    assert (i1.isEmpty)
  }

  "toValue " should " serialize Seq[Int] " in {
    import eu.inn.binders.value._
    val i1 = Seq(1,2,3)
    val d1 = i1.toValue
    d1.asSeq should equal (Seq(1,2,3).map(Number(_)))
  }

  "fromValue " should " deserialize Seq[Int] " in {
    import eu.inn.binders.value._
    val d1 = Lst(Seq(Number(1),Text("2"),Number(3)))
    val i1 = d1.fromValue[Seq[Int]]
    i1 should equal (Seq(1,2,3))
  }

  "toValue " should " serialize Map[String,Int] " in {
    import eu.inn.binders.value._
    val i1: Map[String,Int] = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = i1.toValue
    d1.asMap should equal (i1 map toDynamicNumber)
  }

  "fromValue " should " deserialize Map[String,Int] " in {
    import eu.inn.binders.value._
    val m = Map("a" -> 1, "b" -> 2, "c" -> 3)
    val d1 = Obj(m map toDynamicNumber)
    val i1 = d1.fromValue[Map[String,Int]]
    i1 should equal (m)
  }

  "toValue " should " serialize Obj " in {
    import eu.inn.binders.value._
    val i1 = TestDynamic(1,"ho",true)
    val d1 = i1.toValue
    d1.asMap should equal (Map("a" -> Number(1), "b" -> Text("ho"), "c" -> Bool(true)))
  }

  "fromValue " should " deserialize StringMap = Map[String,String] " in {
    import DefineType._
    import eu.inn.binders.value._

    val m = ObjV("a" -> "he", "b" -> "ho")
    val map = m.fromValue[StringMap]
    map should equal(Map("a"->"he", "b"->"ho"))
  }

  "DynamicValue " should " allow selectDynamic " in {
    val d = ObjV("a" -> 1, "b" -> "ho", "c" -> true, "_" -> false,
      "inner" → ObjV("x" → "100500")
    )
    val a = d.a
    a should equal(Number(1))

    val b = d.b
    b should equal(Text("ho"))

    val f = d.__
    f should equal(Bool(false))

    //val i = d.inner.x // todo: this doesn't work under scala 2.10, uncomment in the future
    //i shouldBe Text("100500")
  }

  "DynamicValue " should " merge (+) " in {
    val value1 = ObjV("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value2 = ObjV("a" -> 2, "b" -> "no", "c" -> false, "d" → Null)
    val value3 = value1 + value2
    value3 should equal(ObjV("a" -> 2, "b" -> "no", "c" -> false, "d" → Null, "e" → Text("kl")))
  }

  "Obj " should " preserve order of fields " in {
    val seq = Seq[(String, Value)]("a" -> 1, "b" -> "ho", "c" -> true, "d" → 5, "e" → "kl")
    val value1 = ObjV(seq: _*)

    val z = seq.zipWithIndex.map(a ⇒ a._2 → a._1).toMap

    value1.v.zipWithIndex.foreach{
      case (kv, index) ⇒
        z(index) should equal(kv)
    }
  }

  "implicits" should "do conversion" in {
    val obj = ObjV("a" → 5, "b" → "18")
    obj should equal(Obj(Map("a"→Number(5), "b"→Text("18"))))

    val obj2 = ObjV("a" → "b")
    obj2 should equal(Obj(Map("a"→Text("b"))))

    val lst = Lst(Seq("a",1,false))
    lst should equal(Lst(Seq(Text("a"),Number(1),Bool(false))))
  }

  "Value " should "do pattern matching" in {
    val obj = ObjV("a" → 5, "b" → "18")
    obj match {
      case Obj(map) ⇒ // fine
    }

    val lst: Value = LstV("a",1,false)
    lst match {
      case Lst(seq) ⇒ // fine
      case Bool(_) ⇒ fail
      case Null ⇒ fail
      case Number(_) ⇒ fail
      case Obj(_) ⇒ fail
      case Text(_) ⇒ fail
    }
  }

  def toDynamicNumber(kv: (String, Int)) = {
    (kv._1, Number(kv._2))
  }
}
