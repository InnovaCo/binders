import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.binders._
import scala.reflect.ClassTag

class TestBindListSpec extends FlatSpec with Matchers {

  "Case class with List[T] and Set[T] " should " be bound to statement by names " in {
    val m = mock[TestStatement]
    val tcl = TestCollections(List(123456, 7890), Set("aaa", "bbb"), Map(1l -> "a", 2l -> "b"))
    m.bind(0, tcl)
    verify(m).setList("intLst", List(123456, 7890))
    verify(m).setSet("strSet", Set("aaa", "bbb"))
    verify(m).setMap("longStrMap", Map(1l -> "a", 2l -> "b"))
  }

  "Case class with List[T] and Set[T] " should " be unbound from row" in {
    val m = mock[TestRow]
    when(m.getList[Int]("intLst")).thenReturn(List(123456, 7890))
    when(m.getSet[String]("strSet")).thenReturn(Set("aaa", "bbb"))
    when(m.getMap[Long, String]("longStrMap")).thenReturn(Map(1l -> "a", 2l -> "b"))
    val t = m.unbind[TestCollections]
    assert(t === TestCollections(List(123456, 7890), Set("aaa", "bbb"), Map(1l -> "a", 2l -> "b")))
  }
}