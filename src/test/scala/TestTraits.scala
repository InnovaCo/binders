import eu.inn.binders.Query

trait TestRow extends eu.inn.binders.Row {
  def getInt(name: String) : Int
  def getIntNullable(name: String) : Option[Int]
}

trait TestRows extends eu.inn.binders.Rows[TestRow] {

}

trait TestStatement extends eu.inn.binders.Statement {
  /*def setLong(index: Int, value: Long)
  def setLongNullable(index: Int, value: Option[Long])
  def setLong(name: String, value: Long)
  def setLongNullable(name: String, value: Option[Long])*/
  def setInt(index: Int, value: Int)
  def setIntNullable(index: Int, value: Option[Int])
  def setInt(name: String, value: Int)
  def setIntNullable(name: String, value: Option[Int])
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