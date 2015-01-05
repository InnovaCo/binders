import eu.inn.binders.naming.PlainConverter
import java.util._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestDateSpec extends FlatSpec with Matchers {
  val (yesterday, now) = {
    val cal = Calendar.getInstance()
    cal.setTime(new Date())
    cal.add(Calendar.DATE, -11)
    (cal.getTime(), new Date())
  }

  "all java.util.Date parameters " should " bind" in {
    val m = mock[TestSerializer[PlainConverter]]
    m.bind(yesterday)
    m.bind(Some(yesterday))
    m.bind(now)
    verify(m).addDate(yesterday)
    verify(m).addDateNullable(Some(yesterday))
    verify(m).addDate(now)
    verifyNoMoreInteractions(m)
  }

  "java.util.Date " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter]]
    when (m.getDate()).thenReturn(yesterday)
    when (m.getDateNullable()).thenReturn(Some(now))
    val d1 = m.unbind[Date]
    val d2 = m.unbind[Option[Date]]
    assert (d1 === yesterday)
    assert (d2 === Some(now))
  }
}