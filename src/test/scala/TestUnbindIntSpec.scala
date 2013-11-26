import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestUnbindIntSpec extends FlatSpec with Matchers {
  /*"case class with int " should " be created from row by field names" in {
    val m = mock[TestRow]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenReturn(789)
    val t = m.unbind[TestInt]
    assert(t === TestInt(123456,Some(555), 789))
  }

  "case class with int " should " be filled from row by field names and copied" in {
    val m = mock[TestRow]
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

  "case class with int " should " be created from first row of rows" in {
    val m = mock[TestRow]
    when(m.getInt("intValue1")).thenReturn(123456)
    when(m.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m.getInt("intValue2")).thenReturn(789)

    val m2 = mock[TestRows]
    when(m2.iterator).thenReturn(Seq(m).toIterator)

    val t = m2.unbindOne[TestInt]
    assert(t === Some(TestInt(123456,Some(555), 789)))
  }

  "case class with int " should " be created from all of the rows" in {
    val m1 = mock[TestRow]
    when(m1.getInt("intValue1")).thenReturn(123456)
    when(m1.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m1.getInt("intValue2")).thenReturn(789)

    val m2 = mock[TestRow]
    when(m2.getInt("intValue1")).thenReturn(654321)
    when(m2.getIntNullable("nullableValue")).thenReturn(None)
    when(m2.getInt("intValue2")).thenReturn(987)

    val m3 = mock[TestRows]
    when(m3.iterator).thenReturn(Seq(m1,m2).toIterator)

    val t = m3.unbindAll[TestInt].toSeq
    assert(t === Seq(TestInt(123456,Some(555), 789), TestInt(654321, None, 987)))
  }*/
}