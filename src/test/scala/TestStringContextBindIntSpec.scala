
/*

see todo in TestObjectDontUse

import eu.inn.binders.internal.TestObjectDontUse._

class TestStringContextBindIntSpec extends FlatSpec with Matchers {

  "all case class fields with int " should " be bound to statement by names " in {
    implicit val m = mock[TestStatement[PlainConverter]]

    val intValue1=123456
    val nullableValue=Some(555)
    val intValue2=7890
    test"1:$intValue1 2:$nullableValue 3:$intValue2"

    verify(m).setInt(0, 123456)
    verify(m).setIntNullable(1, Some(555))
    verify(m).setInt(2, 7890)
    verifyNoMoreInteractions(m)
  }
}

*/