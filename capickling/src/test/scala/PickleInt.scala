import com.datastax.driver.core.{Row, BoundStatement}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._
import scala.pickling._
import eu.inn.capickling._

class PickleInt extends FlatSpec with Matchers {
  /*
  "An integer parameter " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    123456.pickleTo(out)
    890.pickleTo(out)
    verify(m).setInt(0,123456)
    verify(m).setInt(1,890)
  }

  case class TestInt(intValue: Int)
  "An integer inside case class " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    TestInt(123456).pickleTo(out)
    verify(m).setInt("intValue",123456)
  }

  "An integer " should " be read from " in {
    val m = mock[Row]
    when(m.getInt("intValue")).thenReturn(123456)
    val t = m.unpickle[TestInt]
    t.intValue should be(123456)
  }

  "An Option[Int] parameter " should " be bound " in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    Some(123456).pickleTo(out)
    None.pickleTo(out)
    555.pickleTo(out)
    verify(m).setInt(0,123456)
    verify(m).setInt(2,555)
    verifyNoMoreInteractions(m)
  }

  case class TestOptionInt(a: Int, b: Option[Int], c: Int, d: Option[Int], e: Int)

  "An Option[Int] parameter inside case class " should " be bound" in {
    val m = mock[BoundStatement]
    val out = new BoundStatementOutput(m)
    val t = TestOptionInt(123, Some(456), 789, None, 98)
    t.pickleTo(out)
    verify(m).setInt("a",123)
    verify(m).setInt("b",456)
    verify(m).setInt("c",789)
    verify(m).setInt("e",98)
    verifyNoMoreInteractions(m)
  }
  */

  case class TestOptionInt(a: Int, b: Option[Int], c: Int, d: Option[Int], e: Int)

  "An Option[Int] " should " be read from " in {
    val m = mock[Row]
    when(m.getInt("a")).thenReturn(123)
    when(m.getInt("b")).thenReturn(456)
    when(m.getInt("c")).thenReturn(789)
    when(m.isNull("d")).thenReturn(true)
    when(m.getInt("e")).thenReturn(98)

    val t = m.unpickle[TestOptionInt]
    t should be(TestOptionInt(123,Some(456),789,None,98))
  }
}