import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestUnbind extends FlatSpec with Matchers {

  case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

  trait InputClass extends eu.inn.binders.Row {
    def getInt(name: String) : Int
    def getIntNullable(name: String) : Option[Int]
  }

  "case class with int " should " be created from row by field names" in {
    val m = mock[InputClass]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenReturn(789)
    val t = m.unbind[TestInt]
    assert(t === TestInt(123456,Some(555), 789))
  }

  "case class with int " should " be created from row by field names implicitly" in {
    val m = mock[InputClass]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenReturn(789)
    val t = m.unbind[TestInt]
    assert(t === TestInt(123456,Some(555), 789))
  }


  "case class with int " should " be filled from row by field names and copied" in {
    val m = mock[InputClass]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenThrow(new RuntimeException("There is no field intValue2 in a row"))
    when(m.hasField("intValue1")).thenReturn(true)
    when(m.hasField("nullableValue")).thenReturn(true)
    when(m.hasField("intValue2")).thenReturn(false)
    val t1 = TestInt(0,Some(0),7890)
    val t = m.unbindPartial(t1)
    assert(t === TestInt(123456,Some(555), 7890))
  }
}