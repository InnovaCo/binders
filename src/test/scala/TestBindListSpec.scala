import eu.inn.binders.naming.PlainConverter
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestBindListSpec extends FlatSpec with Matchers {

  "Case class with List[T] and Set[T] " should " be bound" in {
    val m = mock[TestSerializer[PlainConverter]]
    val list: List[Int] = List(123456, 7890)
    val set: Set[String] = Set("aaa", "bbb")
    val map: Map[Long,String] = Map(1l -> "a", 2l -> "b")
    m.bindArgs(list, set, map)
    verify(m).addList(List(123456, 7890))
    verify(m).addSet(Set("aaa", "bbb"))
    verify(m).addMap(Map(1l -> "a", 2l -> "b"))
  }

  "List of integers without explicit type " should " be bound" in {
    val m = mock[TestSerializer[PlainConverter]]
    m.bindArgs(List(123456, 7890))
    verify(m).addList(List(123456, 7890))
  }

  "Case class with List[T] and Set[T] " should " be deserialized" in {
    val m = mock[TestDeserializer[PlainConverter]]
    when(m.getList[Int]("intLst")).thenReturn(List(123456, 7890))
    when(m.getSet[String]("strSet")).thenReturn(Set("aaa", "bbb"))
    when(m.getMap[Long, String]("longStrMap")).thenReturn(Map(1l -> "a", 2l -> "b"))
    val t = m.unbind[TestCollections]
    assert(t === TestCollections(List(123456, 7890), Set("aaa", "bbb"), Map(1l -> "a", 2l -> "b")))
  }

  /*
  "Case class with Option[Map[K,V]] " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter]]
    m.bindArgs(Some(Map("1" -> Set(5, 6, 7))))
    verify(m).addMapNullable(Some(Map("1" -> Set(5, 6, 7))))
  }*/

  "Case class with Option[Map[K,V]] " should " be deserialized" in {
    val m = mock[TestDeserializer[PlainConverter]]
    when(m.getGenericMap[String, Set[Int]]("genericMap")).thenReturn(Some(Map("1" -> Set(5, 6, 7))))
    when(m.getGenericMap("genericMapNone")).thenReturn(None)
    val t = m.unbind[TestGenericCollections]
    assert(t === TestGenericCollections(Some(Map("1" -> Set(5, 6, 7))), None))
  }
}