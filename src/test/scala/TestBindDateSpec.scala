import eu.inn.binders.naming.PlainConverter
import java.util._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestBindDateSpec extends FlatSpec with Matchers {
  val (yesterday, now) = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.DATE, -11)
    (cal.getTime(), new Date())
  }

  "all java.util.Date parameters " should " be bound to statement by indexes " in {
    val m = mock[TestSerializer[PlainConverter]]
    m.bind(yesterday)
    m.bind(Some(yesterday))
    m.bind(now)
    verify(m).addDate(yesterday)
    verify(m).addDateNullable(Some(yesterday))
    verify(m).addDate(now)
    verifyNoMoreInteractions(m)
  }
}