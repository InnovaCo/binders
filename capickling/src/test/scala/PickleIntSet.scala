import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito.{verify,when}
import scala.pickling._
import eu.inn.capickling._
import scala.collection.JavaConversions._

class PickleIntSet extends FlatSpec with Matchers {

  case class TestSetInt(setIntSet: Set[Int])
  "A set of int " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    TestSetInt(Set(1,2,3,4,5,6)).pickleTo(out)
    verify(m).setSet("setIntSet", Set(1,2,3,4,5,6))
  }

  "A set of int " should " be read from " in {
    val m = mock[Row]
    when(m.getSet("setIntSet", classOf[Int])).thenReturn(Set(1,2,3,4,5,6))
    val t = m.unpickle[TestSetInt]
    t.setIntSet should be(Set(1,2,3,4,5,6))
  }
}