import eu.inn.binders._
import eu.inn.binders.core.FieldNotFoundException
import eu.inn.binders.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.mockito.Mockito._
import org.mockito.Matchers._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestUnbindInnerSpec extends FlatSpec with Matchers {
  "case class inner class " should " be deserialized by field names" in {

    val m1 = mock[TestDeserializer[PlainConverter]]
    when(m1.getInt("intValue1")).thenReturn(123456)
    when(m1.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m1.getInt("intValue2")).thenReturn(789)

    val m2 = mock[TestDeserializer[PlainConverter]]
    when(m2.getInt("intValue1")).thenReturn(1)
    when(m2.getIntNullable("nullableValue")).thenReturn(None)
    when(m2.getInt("intValue2")).thenReturn(3)

    val mo = mock[TestDeserializer[PlainConverter]]

    when(mo.getNullableFieldDeserializer("inner")).thenReturn(Some(m1))
    when(mo.getNullableFieldDeserializer("nullableInner")).thenReturn(None)
    when(mo.getNullableFieldDeserializer("nullableInner1")).thenReturn(Some(m2))

    //when(mo.getNullableFieldDeserializer(anyString)).thenReturn(None)

    val t = mo.unbind[TestInnerProduct]
    //assert(t === TestInnerClass(TestInt(123456, Some(555), 789)))
    assert(t === TestInnerProduct(TestProduct(123456, Some(555), 789), None, Some(TestProduct(1,None,3))))
  }

  "case class inner class " should " throw FieldNotFound for absent field" in {

    val m1 = mock[TestDeserializer[PlainConverter]]
    when(m1.getInt("intValue1")).thenReturn(123456)
    when(m1.getIntNullable("nullableValue")).thenReturn(Some(555))
    when(m1.getInt("intValue2")).thenReturn(789)

    val m2 = mock[TestDeserializer[PlainConverter]]
    when(m2.getInt("intValue1")).thenReturn(1)
    when(m2.getIntNullable("nullableValue")).thenReturn(None)
    when(m2.getInt("intValue2")).thenReturn(3)

    val mo = mock[TestDeserializer[PlainConverter]]

    when(mo.getNullableFieldDeserializer("inner")).thenReturn(None)
    when(mo.getNullableFieldDeserializer("nullableInner")).thenReturn(None)
    when(mo.getNullableFieldDeserializer("nullableInner1")).thenReturn(None)

    //when(mo.getNullableFieldDeserializer(anyString)).thenReturn(None)

    intercept[FieldNotFoundException] {
      val t = mo.unbind[TestInnerProduct]
    }
  }
}