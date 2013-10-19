import com.datastax.driver.core.{Row, ResultSet, BoundStatement}
import org.scalatest._
import scala.pickling._
import com.maqdev.capickling._
import org.mockito.Mockito._

class CassandraUnpicklingSpec extends FlatSpec with Matchers {

  case class TestInt(intValue: Int)

  "An integer " should " be read from " in {

    val m = org.scalatest.mock.MockitoSugar.mock[Row]
    when(m.getInt("intValue")).thenReturn(123456)

    val t = m.unpickle[TestInt]

    t.intValue should be(123456)
  }
}