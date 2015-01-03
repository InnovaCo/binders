import eu.inn.binders._
import eu.inn.binders.naming.PlainConverter
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestBindIntSpec extends FlatSpec with Matchers {
  "all int parameters " should " be bound to statement by indexes " in {
    val m = mock[TestSerializer[PlainConverter]]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bind(i1)
    m.bind(i2)
    m.bind(i3)
    verify(m).addInt(123456)
    verify(m).addIntNullable(Some(555))
    verify(m).addInt(7890)
    verifyNoMoreInteractions(m)
  }

  "all int parameters " should " be bound to statement as args by indexes " in {
    val m = mock[TestSerializer[PlainConverter]]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bindArgs(i1, i2, i3)
    verify(m).addInt(123456)
    verify(m).addIntNullable(Some(555))
    verify(m).addInt(7890)
    verifyNoMoreInteractions(m)
  }
}
