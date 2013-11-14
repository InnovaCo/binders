import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito.{verify,when}
import scala.pickling._
import eu.inn.capickling._
import scala.collection.JavaConversions._

class PickleLongSet extends FlatSpec with Matchers {

  case class TestSetLong(setLongSet: Set[Long])
  "A set of long " should " be pickled " in {
    val m = mock[BoundStatement]
    TestSetLong(Set(1l,2l,3l,4l,5l,6l)).pickleTo(m)
    verify(m).setSet("setLongSet", Set(1l,2l,3l,4l,5l,6l))
  }

  "A set of long " should " be unpickled from " in {
    val m = mock[Row]
    when(m.getSet("setLongSet", classOf[Long])).thenReturn(Set(1l,2l,3l,4l,5l,6l))
    val t = m.unpickle[TestSetLong]
    t.setLongSet should be(Set(1l,2l,3l,4l,5l,6l))
  }
}