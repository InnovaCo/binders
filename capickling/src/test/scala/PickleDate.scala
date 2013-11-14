import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import scala.pickling._
import eu.inn.capickling._

class PickleDate extends FlatSpec with Matchers {

  case class TestDate(dateValue: java.util.Date)
  "A date " should " be bound " in {
    val m = mock[BoundStatement]
    val d = yesterday
    TestDate(d).pickleTo(m)
    verify(m).setDate("dateValue",d)
  }

  "A date " should " be read from " in {
    val m = mock[Row]
    val d = yesterday
    when(m.getDate("dateValue")).thenReturn(d)
    val t = m.unpickle[TestDate]
    t.dateValue should be(d)
  }

  def yesterday = {
    import java.util._
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.DATE, -11)
    cal.getTime()
  }
}