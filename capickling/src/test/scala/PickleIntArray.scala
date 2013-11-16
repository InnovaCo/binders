import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{verify,when}
import scala.pickling._
import eu.inn.capickling._
import scala.collection.JavaConversions._

class PickleIntArray extends FlatSpec with Matchers {

  case class TestArrayInt(arrayInt: Array[Int])
  "An array of int " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    TestArrayInt(Array(1,2,3,4,5,6)).pickleTo(out)
    verify(m).setList("arrayInt", Array(1,2,3,4,5,6).toList)
  }

  "An array of int " should " be read from " in {
    val m = mock[Row]
    when(m.getList("arrayInt", classOf[Int])).thenReturn(List(1,2,3,4,5,6))
    val t = m.unpickle[TestArrayInt]
    t.arrayInt should be(Array(1,2,3,4,5,6))
  }
}