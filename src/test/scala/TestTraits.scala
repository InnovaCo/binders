import eu.inn.binders.core.{Serializer, Deserializer}
import eu.inn.binders.naming.Converter
import java.util.Date

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

trait TestDeserializer[C <: Converter] extends Deserializer[C] {
  def readInt(): Int

  def readIntNullable(): Option[Int]

  def readDate(): Date

  def readDateNullable(): Option[Date]

  def iterator(): Iterator[TestDeserializer[C]]
}

trait TestDeserializerWithList[C <: Converter] extends Deserializer[C] {
  def readList[T: ClassTag](): List[T] = ???

  def iterator(): Iterator[TestDeserializer[C]]
}

trait TestSerializer[C <: Converter] extends Serializer[C] {
  def writeInt(value: Int)

  def writeIntNullable(value: Option[Int])

  def writeDate(value: Date)

  def writeDateNullable(value: Option[Date])

  def writeList[T: ClassTag](value: List[T])

  def writeSet[T](value: Set[T])

  def writeMap[K, V](value: Map[K, V])

  def writeMapNullable[K, V](value: Option[Map[K, V]])

  def getFieldSerializer(fieldName: String): Option[TestSerializer[C]] = ???
}

case class TestProduct(intValue1: Int, nullableValue: Option[Int], intValue2: Int)

case class TestInnerProduct(inner: TestProduct, nullableInner: Option[TestProduct], nullableInner1: Option[TestProduct])

case class TestCollections(intLst: List[Int], strSet: Set[String], longStrMap: Map[Long, String])

case class TestGenericCollections(genericMap: Option[Map[String, Set[Int]]], genericMapNone: Option[Map[String, Set[Int]]])
