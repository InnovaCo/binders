import eu.inn.binders.naming.PlainConverter
import java.util._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestGenericSuper(x: String) extends TestGeneric

class TestGenericWithBooundsSpec extends FlatSpec with Matchers {
  "Generic with bounds " should " bind refined type" in {
    val m = mock[TestSerializer[PlainConverter]]
    val g = new TestGeneric {}
    m.bind(g)
    verify(m).writeGenericWithBounds(g)
    verifyNoMoreInteractions(m)
  }

  "Generic with bounds " should " bind" in {
    val m = mock[TestSerializer[PlainConverter]]
    val g = new TestGenericSuper("1")
    m.bind(g)
    verify(m).writeGenericWithBounds(g)
    verifyNoMoreInteractions(m)
  }

  "Generic with bounds " should " unbind" in {
    val m = mock[TestDeserializer[PlainConverter]]
    val r = new TestGeneric{}
    when (m.readGenericWihBounds).thenReturn(r)
    val g = m.unbind[TestGeneric]
    assert (g == r)
  }
}