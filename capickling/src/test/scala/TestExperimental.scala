import com.datastax.driver.core.{Row, BoundStatement}
import eu.inn.Binder
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._

class TestExperimental extends FlatSpec with Matchers {

  case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

  class OutputClass {
    def setInt(index: Int, value: Int) = println(index + " = " + value)
    def setIntNullable(index: Int, value: Option[Int]) = println(index + " = " + value)
    def setInt(name: String, value: Int) = println(name + " = " + value)
    def setIntNullable(name: String, value: Option[Int]) = println(name + " = " + value)
  }

  "case class with int " should " serialize " in {
    val m = mock[OutputClass]
    Binder.bindInto(TestInt(123456, Some(555), 7890), m, 0)
    verify(m).setInt("intValue1",123456)
    verify(m).setIntNullable("nullableValue",Some(555))
    verify(m).setInt("intValue2",7890)
    verifyNoMoreInteractions(m)
  }

  "int parameters " should " serialize " in {
    val m = mock[OutputClass]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    Binder.bindInto(i1, m, 0)
    Binder.bindInto(i2, m, 1)
    Binder.bindInto(i3, m, 2)
    verify(m).setInt(0,123456)
    verify(m).setIntNullable(1,Some(555))
    verify(m).setInt(2,7890)
    verifyNoMoreInteractions(m)
  }
}