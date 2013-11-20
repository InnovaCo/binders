import com.datastax.driver.core.{Row, BoundStatement}
import eu.inn.{OutputColumn, Output, Experimental}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._

class TestExperimental extends FlatSpec with Matchers {

  case class AAA(int1:Int, long2:Long)

  case class Column(name: String) extends OutputColumn

  class OutputClass extends Output {

    override val columns: Array[OutputColumn] = Array(Column("int1"), Column("long2"))

    def setInt(index: Int, value: Int) = println(index + " = " + value)
    def setInt(index: Int, value: Option[Int]) = println(index + " = " + value)
    def setInt(name: String, value: Int) = println(name + " = " + value)
    def setInt(name: String, value: Option[Int]) = println(name + " = " + value)
  }

  "A test " should " do something " in {

    val s = Experimental.serialize(AAA(123456, 9l), new OutputClass)

    assert(s == "YAHOO")
  }

}