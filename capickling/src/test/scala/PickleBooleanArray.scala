import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.{verify,when}
import scala.pickling._
import eu.inn.capickling._
import scala.collection.JavaConversions._

class PickleBooleanArray extends FlatSpec with Matchers {
  case class TestArrayBoolean(arrayBool: Array[Boolean])
  "An array of boolean " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    TestArrayBoolean(Array(true,false,true)).pickleTo(out)
    verify(m).setList("arrayBool", Array(true,false,true).toList)
  }

  "An array of boolean " should " be read from " in {
    val m = mock[Row]
    when(m.getList("arrayBool", classOf[Boolean])).thenReturn(List(true,false,true))
    val t = m.unpickle[TestArrayBoolean]
    t.arrayBool should be(Array(true,false,true))
  }
}