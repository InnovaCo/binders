import eu.inn.binders.core.{Serializer, Deserializer}
import eu.inn.binders.naming.Converter
import java.util.Date

trait TestDeserializer[C <: Converter] extends Deserializer[C] {
  //type nameConverterType = C

  def getInt(name: String): Int

  def getIntNullable(name: String): Option[Int]

  def getDate(name: String): Date

  def getDateNullable(name: String): Option[Date]

  def getList[T](name: String): List[T]

  def getSet[T](name: String): Set[T]

  def getMap[K, V](name: String): Map[K, V]

  def getGenericMap[K, V](name: String): Option[Map[K, V]]

  def hasField(fieldName: String): Boolean = ???

  def iterator(): Iterator[TestDeserializer[C]] = ???

  def getAsInt: Int = ???
}

trait TestSerializer[C <: Converter] extends Serializer[C] {
  //type nameConverterType = C

  def addInt(value: Int)

  def addIntNullable(value: Option[Int])

  def setInt(name: String, value: Int)

  def setIntNullable(name: String, value: Option[Int])

  def addDate(value: Date)

  def addDateNullable(value: Option[Date])

  def setDate(name: String, value: Date)

  def setDateNullable(name: String, value: Option[Date])

  def addList[T](value: List[T])

  def setList[T](name: String, value: List[T])

  def addSet[T](value: Set[T])

  def setSet[T](name: String, value: Set[T])

  def setMap[K, V](name: String, value: Map[K, V])

  def setGenericMap[K, V](name: String, value: Option[Map[K, V]])
}

case class TestInt(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

case class TestDate(dateValue1: Date, nullableValue: Option[Date], dateValue2: Date)

case class TestCollections(intLst: List[Int], strSet: Set[String], longStrMap: Map[Long, String])

case class TestGenericCollections(genericMap: Option[Map[String, Set[Int]]], genericMapNone: Option[Map[String, Set[Int]]])
