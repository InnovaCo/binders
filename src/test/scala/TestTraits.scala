import eu.inn.binders.core.{Serializer, Deserializer}
import eu.inn.binders.naming.Converter
import java.util.Date

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

trait TestDeserializer[C <: Converter] extends Deserializer[C] {
  //type nameConverterType = C

  def getInt(): Int

  def getIntNullable(): Option[Int]

  def getDate(): Date

  def getDateNullable(): Option[Date]

  def iterator(): Iterator[TestDeserializer[C]]

  /*



  def getList[T](name: String): List[T]

  def getSet[T](name: String): Set[T]

  def getMap[K, V](name: String): Map[K, V]

  def getGenericMap[K, V](name: String): Option[Map[K, V]]

  def hasField(fieldName: String): Boolean = ???

  def iterator(): Iterator[TestDeserializer[C]] = ???

  def getAsInt: Int = ???

  def getFieldDeserializer(fieldName: String): TestDeserializer[C] = ???

  def getNullableFieldDeserializer(fieldName: String): Option[TestDeserializer[C]] = ???*/
}

trait TestSerializer[C <: Converter] extends Serializer[C] {
  //type nameConverterType = C

  def addInt(value: Int)

  def addIntNullable(value: Option[Int])

  def addDate(value: Date)

  def addDateNullable(value: Option[Date])

  def addList[T: ClassTag](value: List[T])

  def addSet[T](value: Set[T])

  def addMap[K, V](value: Map[K, V])

  def addMapNullable[K, V](value: Option[Map[K, V]])

  def getFieldSerializer(fieldName: String): Option[TestSerializer[C]] = ???
}

case class TestProduct(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

case class TestInnerProduct(inner: TestProduct, nullableInner: Option[TestProduct], nullableInner1: Option[TestProduct])

case class TestCollections(intLst: List[Int], strSet: Set[String], longStrMap: Map[Long, String])

case class TestGenericCollections(genericMap: Option[Map[String, Set[Int]]], genericMapNone: Option[Map[String, Set[Int]]])

//case class TestInnerClass(inner: TestInt/*, nulableInner: Option[TestInt], nulableInner1: Option[TestInt]*/)
