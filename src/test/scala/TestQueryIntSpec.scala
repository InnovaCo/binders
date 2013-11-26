import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import eu.inn.binders._

class TestQueryIntSpec extends FlatSpec with Matchers {
  /*
  "TestQuery " should " should bind parameters by index" in {
    val m =  mock[TestStatement]
    val q = new TestQuery(m)
    val noneInt : Option[Int] = None
    q.execute(10, Some(3), noneInt )
    verify(m).setInt(0,10)
    verify(m).setIntNullable(1,Some(3))
    verify(m).setIntNullable(2, None)
  }

  "TestQuery " should " should bind parameters from case class by name" in {
    val m =  mock[TestStatement]
    val q = new TestQuery(m)
    q.execute(TestInt(10, Some(555), 20))
    verify(m).setInt("intValue1",10)
    verify(m).setIntNullable("nullableValue",Some(555))
    verify(m).setInt("intValue2",20)
  }
  */
}