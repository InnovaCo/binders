import eu.inn.binders._
import eu.inn.binders.naming.{CamelCaseToSnakeCaseConverter, PlainConverter}
import org.mockito.Mockito._
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec, Matchers}

class TestProductSpec extends FlatSpec with Matchers {

  "all case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter]]
    val m1 = mock[TestSerializer[PlainConverter]]
    val m2 = mock[TestSerializer[PlainConverter]]
    val m3 = mock[TestSerializer[PlainConverter]]

    when(m.getFieldSerializer("intValue1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullableValue")).thenReturn(Some(m2))
    when(m.getFieldSerializer("intValue2")).thenReturn(Some(m3))

    m.bind(TestProduct(123456, Some(555), 7890))

    verify(m).getFieldSerializer("intValue1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullableValue")
    verify(m2).writeIntNullable(Some(555))
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("intValue2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)

    verifyNoMoreInteractions(m)
  }

  "all case class fields " should " be serialized by names with specified convention" in {
    val m = mock[TestSerializer[CamelCaseToSnakeCaseConverter]]
    val m1 = mock[TestSerializer[CamelCaseToSnakeCaseConverter]]
    val m2 = mock[TestSerializer[CamelCaseToSnakeCaseConverter]]
    val m3 = mock[TestSerializer[CamelCaseToSnakeCaseConverter]]

    when(m.getFieldSerializer("int_value1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullable_value")).thenReturn(Some(m2))
    when(m.getFieldSerializer("int_value2")).thenReturn(Some(m3))

    m.bind(TestProduct(123456, Some(555), 7890))

    verify(m).getFieldSerializer("int_value1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullable_value")
    verify(m2).writeIntNullable(Some(555))
    verifyNoMoreInteractions(m2)

    verify(m).getFieldSerializer("int_value2")
    verify(m3).writeInt(7890)
    verifyNoMoreInteractions(m3)

    verifyNoMoreInteractions(m)
  }

  "some case class fields " should " be serialized by names " in {
    val m = mock[TestSerializer[PlainConverter]]
    val m1 = mock[TestSerializer[PlainConverter]]

    when(m.getFieldSerializer("intValue1")).thenReturn(Some(m1))
    when(m.getFieldSerializer("nullableValue")).thenReturn(None)
    when(m.getFieldSerializer("intValue2")).thenReturn(None)

    m.bindPartial(TestProduct(123456, Some(555), 7890))

    verify(m).getFieldSerializer("intValue1")
    verify(m1).writeInt(123456)
    verifyNoMoreInteractions(m1)

    verify(m).getFieldSerializer("nullableValue")
    verify(m).getFieldSerializer("intValue2")

    verifyNoMoreInteractions(m)
  }

  "all case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]

    val m1 = mock[TestDeserializer[PlainConverter]]
    when(m1.fieldName).thenReturn(Some("intValue1"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[PlainConverter]]
    when(m2.fieldName).thenReturn(Some("nullableValue"))
    when(m2.readIntNullable()).thenReturn(Some(555))

    val m3 = mock[TestDeserializer[PlainConverter]]
    when(m3.fieldName).thenReturn(Some("intValue2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m2,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[TestProduct]
    assert(t === TestProduct(123456, Some(555), 7890))
  }

  "all case class fields " should " be deserialized by names with specified convention" in {
    val m = mock[TestDeserializer[CamelCaseToSnakeCaseConverter]]

    val m1 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter]]
    when(m1.fieldName).thenReturn(Some("int_value1"))
    when(m1.readInt()).thenReturn(123456)

    val m2 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter]]
    when(m2.fieldName).thenReturn(Some("nullable_value"))
    when(m2.readIntNullable()).thenReturn(Some(555))

    val m3 = mock[TestDeserializer[CamelCaseToSnakeCaseConverter]]
    when(m3.fieldName).thenReturn(Some("int_value2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m2,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbind[TestProduct]
    assert(t === TestProduct(123456, Some(555), 7890))
  }

  "some case class fields " should " be deserialized by names " in {
    val m = mock[TestDeserializer[PlainConverter]]

    val m1 = mock[TestDeserializer[PlainConverter]]
    when(m1.fieldName).thenReturn(Some("intValue1"))
    when(m1.readInt()).thenReturn(123456)

    val m3 = mock[TestDeserializer[PlainConverter]]
    when(m3.fieldName).thenReturn(Some("intValue2"))
    when(m3.readInt()).thenReturn(7890)

    val mi = List(m1,m3)
    when(m.iterator()).thenReturn(mi.toIterator)

    val t = m.unbindPartial(TestProduct(-1, Some(555), -2))
    assert(t === TestProduct(123456, Some(555), 7890))
  }
}
