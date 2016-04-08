import eu.inn.binders.core.BindOptions
import eu.inn.binders.naming.PlainConverter
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

case class TestStringCollections(seq: Seq[String], map: Map[String, String])
case class TestStringListCls(list: List[String])
case class TestStringVectorCls(list: Vector[String])
case class TestStringIndexedSeqCls(list: IndexedSeq[String])
case class TestStringSetCls(list: Set[String])
case class TestArrayCls(array: Array[String])

class TestCollectionsInClassSpec extends FlatSpec with Matchers {

  "all case class collection fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter]]
    val m1 = mock[TestSerializer[PlainConverter]]
    val m2 = mock[TestSerializer[PlainConverter]]
    val m3 = mock[TestSerializer[PlainConverter]]

    when(m.getFieldSerializer("seq")).thenReturn(Some(m1))
    when(m.getFieldSerializer("map")).thenReturn(Some(m2))
    when(m2.getFieldSerializer("x")).thenReturn(Some(m3))

    val col = TestStringCollections(Seq("a","b"), Map("x" → "y"))
    m.bind(col)

    verify(m).beginObject()
    verify(m).getFieldSerializer("seq")
    verify(m1).beginArray()
    verify(m1).writeString("a")
    verify(m1).writeString("b")
    verify(m1).endArray()
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("map")
    verify(m2).beginObject()
    verify(m2).getFieldSerializer("x")
    verify(m3).writeString("y")
    verifyNoMoreInteractions(m3)
    verify(m2).endObject()
    verifyNoMoreInteractions(m2)

    verify(m).endObject()
    verifyNoMoreInteractions(m)
  }

  "empty class collection fields " should " NOT be skipped according to option" in {
    val m = mock[TestSerializer[PlainConverter]]
    val m1 = mock[TestSerializer[PlainConverter]]
    val m2 = mock[TestSerializer[PlainConverter]]

    when(m.getFieldSerializer("seq")).thenReturn(Some(m1))
    when(m.getFieldSerializer("map")).thenReturn(Some(m2))

    m.bind(TestStringCollections(Seq.empty, Map.empty))

    verify(m).beginObject()
    verify(m).getFieldSerializer("seq")
    verify(m1).beginArray()
    verify(m1).endArray()
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("map")
    verify(m2).beginObject()
    verify(m2).endObject()
    verifyNoMoreInteractions(m2)

    verify(m).endObject()
    verifyNoMoreInteractions(m)
  }

  "empty class collection fields " should " be skipped according to option" in {
    val m = mock[TestSerializer[PlainConverter]]
    val m1 = mock[TestSerializer[PlainConverter]]
    val m2 = mock[TestSerializer[PlainConverter]]

    when(m.getFieldSerializer("seq")).thenReturn(Some(m1))
    when(m.getFieldSerializer("map")).thenReturn(Some(m2))

    implicit val bo: BindOptions = new BindOptions(true)
    m.bind(TestStringCollections(Seq.empty, Map.empty))

    verify(m).beginObject()
    verify(m).endObject()
    verifyNoMoreInteractions(m)
  }

  "all case class collections " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]

    val m1 = mock[TestDeserializer[PlainConverter]]
    when(m1.fieldName).thenReturn(Some("seq"))
    val miSeq = List("a","b") map { s ⇒
      val me = mock[TestDeserializer[PlainConverter]]
      when(me.readString()).thenReturn(s)
      me
    }
    when(m1.iterator()).thenReturn(miSeq.toIterator)

    val mci = List(m1)
    when(m.iterator()).thenReturn(mci.toIterator)

    val t = m.unbind[TestStringCollections]
    assert(t === TestStringCollections(Seq("a","b"), Map.empty))
  }

  "empty List in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]
    val mci = List()
    when(m.iterator()).thenReturn(mci.toIterator)

    val t = m.unbind[TestStringListCls]
    assert(t === TestStringListCls(List.empty))
  }

  "empty Vector in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]
    val mci = List()
    when(m.iterator()).thenReturn(mci.toIterator)

    val t = m.unbind[TestStringVectorCls]
    assert(t === TestStringVectorCls(Vector.empty))
  }

  "empty IndexedSeq in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]
    val mci = List()
    when(m.iterator()).thenReturn(mci.toIterator)

    val t = m.unbind[TestStringIndexedSeqCls]
    assert(t === TestStringIndexedSeqCls(IndexedSeq.empty))
  }

  "empty Set in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]
    val mci = List()
    when(m.iterator()).thenReturn(mci.toIterator)

    val t = m.unbind[TestStringSetCls]
    assert(t === TestStringSetCls(Set.empty))
  }

  "empty Array in case class " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]
    val mci = List()
    when(m.iterator()).thenReturn(mci.toIterator)

    //val x: Iterator[String]
    //x.toArray

    val t = m.unbind[TestArrayCls]
    t shouldBe TestArrayCls(Array[String]())
  }
}
