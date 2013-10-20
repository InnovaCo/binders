import com.datastax.driver.core.BoundStatement
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.mock.MockitoSugar._
import org.mockito.Mockito.verify
import scala.pickling._
import com.maqdev.capickling._
import scala.collection.JavaConversions._

class CassandraPicklingSpec extends FlatSpec with Matchers {

  case class TestInt(intValue: Int)
  "An integer " should " be bound " in {
    val m = mock[BoundStatement]
    TestInt(123456).pickleTo(m)
    verify(m).setInt("intValue",123456)
  }

  case class TestArrayInt(arrayInt: Array[Int])
  "An array of int " should " be bound " in {
    val m = mock[BoundStatement]
    TestArrayInt(Array(1,2,3,4,5,6)).pickleTo(m)
    verify(m).setList("arrayInt", Array(1,2,3,4,5,6).toList)
  }
}