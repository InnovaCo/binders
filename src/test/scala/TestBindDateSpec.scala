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

  "case class with java.util.Date " should "  be bound to statement by names " in {
    val m = mock[TestStatement[PlainConverter]]
    m.bind(0, TestDate(yesterday, Some(yesterday), now))
    verify(m).setDate("dateValue1", yesterday)
    verify(m).setDateNullable("nullableValue", Some(yesterday))
    verify(m).setDate("dateValue2", now)
    verifyNoMoreInteractions(m)
  }

  "all java.util.Date parameters " should " be bound to statement by indexes " in {
    val m = mock[TestStatement[PlainConverter]]
    m.bind(0, yesterday)
    m.bind(1, Some(yesterday))
    m.bind(2, now)
    verify(m).setDate(0, yesterday)
    verify(m).setDateNullable(1, Some(yesterday))
    verify(m).setDate(2, now)
    verifyNoMoreInteractions(m)
  }

  "some case class with java.util.Date " should " be bound to statement by names " in {
    val m = mock[TestStatement[PlainConverter]]
    when(m.hasParameter("dateValue1")).thenReturn(true)
    when(m.hasParameter("nullableValue")).thenReturn(true)
    when(m.hasParameter("dateValue2")).thenReturn(false)
    m.bindPartial(0, TestDate(yesterday, Some(yesterday), now))
    verify(m).setDate("dateValue1", yesterday)
    verify(m).setDateNullable("nullableValue", Some(yesterday))
    verify(m, times(0)).setDate("dateValue2", now)
  }
}