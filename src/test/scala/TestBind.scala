import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestBind extends FlatSpec with Matchers {

  case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

  trait TestStatement extends eu.inn.binders.Statement {
    def setLong(index: Int, value: Long)
    def setLongNullable(index: Int, value: Option[Long])
    def setLong(name: String, value: Long)
    def setLongNullable(name: String, value: Option[Long])
    def setInt(index: Int, value: Int)
    def setIntNullable(index: Int, value: Option[Int])
    def setInt(name: String, value: Int)
    def setIntNullable(name: String, value: Option[Int])
  }
  
  

  "all case class with int " should "  be bound to statement by names " in {
    val m = mock[TestStatement]
    m.bind(0, TestInt(123456, Some(555), 7890))
    verify(m).setInt("intValue1",123456)
    verify(m).setIntNullable("nullableValue",Some(555))
    verify(m).setInt("intValue2",7890)
    verifyNoMoreInteractions(m)
  }

  "all int parameters " should " be bound to statement by indexes " in {
    val m = mock[TestStatement]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    m.bind(0, i1)
    m.bind(1, i2)
    m.bind(2, i3)
    verify(m).setInt(0,123456)
    verify(m).setIntNullable(1,Some(555))
    verify(m).setInt(2,7890)
    verifyNoMoreInteractions(m)
  }

  "some case class with int " should "  be bound to statement by names " in {
    val m = mock[TestStatement]
    when(m.hasParameter("intValue1")).thenReturn(true)
    when(m.hasParameter("nullableValue")).thenReturn(true)
    when(m.hasParameter("intValue2")).thenReturn(false)
    m.bindPartial(0, TestInt(123456, Some(555), 7890))
    verify(m).setInt("intValue1",123456)
    verify(m).setIntNullable("nullableValue",Some(555))
    verify(m, times(0)).setInt("intValue2",7890)
  }
}