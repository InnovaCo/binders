import eu.inn.binders.naming.PlainConverter
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

/* todo: cover these tests
case class TestCollections(intLst: List[Int], strSet: Set[String], longStrMap: Map[Long, String], intSetN: Option[Set[Int]])

//case class TestGenericCollections(genericMap: Option[Map[String, Set[Int]]], genericMapNone: Option[Map[String, Set[Int]]])


class TestCollectionsInClassSpec extends FlatSpec with Matchers {

  def getMockList = {
    val testData = List(123456, 7890)
    val m = mock[TestDeserializer[PlainConverter]]
    val mi = testData.map {
      l =>
      {
        val mi = mock[TestDeserializer[PlainConverter]]
        when(mi.readInt()).thenReturn(l)
        mi
      }
    }
    when(m.iterator()).thenReturn(mi.toIterator)
    m
  }

  "TestCollections " should " be bound" in {
    val m = mock[TestSerializer[PlainConverter]]

    val t = TestCollections(
      List(3,5),
      Set("x","y"),
      Map(1l -> "yo", 2l -> "ca")
    )

    m.bind(t)
  }

  "TestCollections " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter]]
    val l = m.unbind[TestCollections]
    //assert (l === Seq(123456, 7890))
  }
}

*/