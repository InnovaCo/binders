import eu.inn.binders.core.{Statement, Rows, Row, Query}
import java.util.Date
import scala.reflect._

trait TestRow extends Row {
  def getInt(name: String) : Int
  def getIntNullable(name: String) : Option[Int]
  def getDate(name: String) : Date
  def getDateNullable(name: String) : Option[Date]

  /*def getList[T: ClassTag](name: String) : List[T]
  def getSet[T: ClassTag](name: String) : Set[T]
  def getMap[K: ClassTag, V: ClassTag](name: String) : Map[K,V]*/

  def getList[T](name: String) : List[T]
  def getSet[T](name: String) : Set[T]
  def getMap[K,V](name: String) : Map[K,V]
}

trait TestRows extends Rows[TestRow] {

}

trait TestStatement extends Statement {
  def setInt(index: Int, value: Int)
  def setIntNullable(index: Int, value: Option[Int])
  def setInt(name: String, value: Int)
  def setIntNullable(name: String, value: Option[Int])
  def setDate(index: Int, value: Date)
  def setDateNullable(index: Int, value: Option[Date])
  def setDate(name: String, value: Date)
  def setDateNullable(name: String, value: Option[Date])
  def setList[T](index: Int, value: List[T])
  def setList[T](name: String, value: List[T])
  def setSet[T](index: Int, value: Set[T])
  def setSet[T](name: String, value: Set[T])
  def setMap[K,V](name: String, value: Map[K,V])
}

class TestQuery(statement : TestStatement) extends Query[TestRows, TestStatement]{
  def bindAndExecute(f : TestStatement => Unit) : TestRows = {
    f(statement)
    new Object with TestRows {
      override def iterator = Iterator.empty
    }
  }
}

case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)
case class TestDate(dateValue1: Date, nullableValue: Option[Date], dateValue2: Date)
case class TestCollections(intLst: List[Int], strSet: Set[String], longStrMap: Map[Long,String])
