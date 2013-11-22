import eu.inn.binders.Binder
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._

class TestIntBinder extends FlatSpec with Matchers {

  case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

  trait OutputClass {
    def setInt(index: Int, value: Int)
    def setIntNullable(index: Int, value: Option[Int])
    def setInt(name: String, value: Int)
    def setIntNullable(name: String, value: Option[Int])
  }

  trait InputClass {
    def getInt(name: String) : Int
    def getIntNullable(name: String) : Option[Int]
  }

  trait InputClass2 extends eu.inn.binders.Row {
    def getInt(name: String) : Int
    def getIntNullable(name: String) : Option[Int]
  }

  "case class with int " should "  be bound to statement by names " in {
    val m = mock[OutputClass]
    Binder.bindInto(m, 0, TestInt(123456, Some(555), 7890))
    verify(m).setInt("intValue1",123456)
    verify(m).setIntNullable("nullableValue",Some(555))
    verify(m).setInt("intValue2",7890)
    verifyNoMoreInteractions(m)
  }

  "int parameters " should " be bound to statement by indexes " in {
    val m = mock[OutputClass]
    val i1 = 123456
    val i2 = Some(555)
    val i3 = 7890
    Binder.bindInto(m, 0, i1)
    Binder.bindInto(m, 1, i2)
    Binder.bindInto(m, 2, i3)
    verify(m).setInt(0,123456)
    verify(m).setIntNullable(1,Some(555))
    verify(m).setInt(2,7890)
    verifyNoMoreInteractions(m)
  }

  "case class with int " should " be created from row by field names " in {
    val m = mock[InputClass2]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenThrow(new RuntimeException("There is no field intValue2 in a row"))
    when(m.hasField("intValue1")).thenReturn(true)
    when(m.hasField("nullableValue")).thenReturn(true)
    when(m.hasField("intValue2")).thenReturn(false)
    val t1 = TestInt(0,Some(0),7890)

    val t = Binder.fillFrom[InputClass2, TestInt](m, t1)
    assert(t === TestInt(123456,Some(555), 7890))
  }

  "case class with int " should " be filled from row by field names and copied" in {
    val m = mock[InputClass]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenReturn(789)
    val t = Binder.createFrom[InputClass, TestInt](m)
  }
}