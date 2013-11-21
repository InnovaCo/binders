import com.datastax.driver.core.{Row, BoundStatement}
import eu.inn.{Output, Experimental}
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.{FlatSpec,Matchers}
import org.mockito.Mockito._

class TestExperimental extends FlatSpec with Matchers {

  case class AAA(int1:Int, long2:Option[Long])

  //case class Column(name: String) extends OutputColumn

  class OutputClass extends Output {

    //override val columns: Array[OutputColumn] = Array(Column("int1"), Column("long2"))

    def setInt(index: Int, value: Int) = println(index + " = " + value)
    def setIntN(index: Int, value: Option[Int]) = println(index + " = " + value)
    def setInt(name: String, value: Int) = println(name + " = " + value)
    def setIntN(name: String, value: Option[Int]) = println(name + " = " + value)


    def setLong(index: Int, value: Long) = println(index + " = " + value)
    def setLongN(index: Int, value: Option[Long]) = println(index + " = " + value)
    //def setInt(name: String, value: Int) = println(name + " = " + value)
    //def setInt(name: String, value: Option[Int]) = println(name + " = " + value)
  }

  "A test " should " do something " in {

    val a = AAA(123456, Some(9l))
    val out = new OutputClass
    val s = Experimental.serialize(a, out)

    assert(s == "YAHOO")
  }

}