import com.datastax.driver.core.BoundStatement
import org.scalatest._
import scala.pickling._
import com.maqdev.capickling._

class CassandraPicklingSpec extends FlatSpec with Matchers {

  case class TestInt(intValue: Int)

  "An integer " should " be bound " in {

    val m = org.scalatest.mock.MockitoSugar.mock[BoundStatement]
    val p = pickleFormat.createBuilder(m)
    TestInt(123456).pickleInto(p)

    org.mockito.Mockito.verify(m).setInt("intValue",123456)
  }
}