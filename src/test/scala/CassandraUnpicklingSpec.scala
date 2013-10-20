import com.datastax.driver.core.{Row, ResultSet, BoundStatement}
import org.mockito.Mockito.{when, verify}
import org.scalatest.{FlatSpec,Matchers}
import org.scalatest.mock.MockitoSugar._
import com.maqdev.capickling._
import scala.collection.JavaConversions._

class CassandraUnpicklingSpec extends FlatSpec with Matchers {

  case class TestInt(intValue: Int)

  /*"An integer " should " be read from " in {
    val m = mock[Row]
    when(m.getInt("intValue")).thenReturn(123456)
    val t = m.unpickle[TestInt]
    t.intValue should be(123456)
  }*/

  case class TestArrayInt(arrayInt: Array[Int])
  "An array of int " should " be read from " in {
    val m = mock[Row]
    when(m.getList("arrayInt", classOf[Int])).thenReturn(List(1,2,3,4,5,6))
    val t = m.unpickle[TestArrayInt]
    t.arrayInt should be(Array(1,2,3,4,5,6))
  }
}