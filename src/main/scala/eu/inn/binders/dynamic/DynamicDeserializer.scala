package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Deserializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicDeserializeException(message: String) extends RuntimeException(message)

class DynamicDeserializerBase[C <: Converter, I <: Deserializer[C]] (dynamic: DynamicObject, val fieldName: Option[String])
  extends Deserializer[C] {

  def iterator(): Iterator[I] = {
    if (dynamic.typ == DynamicType.Seq) {
      dynamic.asSeq.toIterator.map(createFieldDeserializer(_, None))
    }
    else if (dynamic.typ == DynamicType.Map) {
      dynamic.asMap.toIterator.map(kv => createFieldDeserializer(kv._2, Some(kv._1)))
    }
    else
      throw new DynamicDeserializeException("Couldn't iterate on: " + dynamic)
  }

  protected def createFieldDeserializer(dynamic: DynamicObject, fieldName: Option[String]): I = ??? //new JsonDeserializer[C](jsonNode, fieldName)
  
  def isNull: Boolean = dynamic.isNull
  def readString(): String = dynamic.asString
  def readInt(): Int = dynamic.asInt
  def readLong(): Long = dynamic.asLong
  def readDouble(): Double = dynamic.asDouble
  def readFloat(): Float = dynamic.asFloat
  def readBoolean(): Boolean = dynamic.asBoolean
  def readBigDecimal(): BigDecimal = dynamic.asBigDecimal
  def readDate(): Date = dynamic.asDate
//  def readMap(): Map[String,DynamicObject] = dynamic.asMap
//  def readSeq(): Seq[DynamicObject] = dynamic.asSeq
}

class DynamicDeserializer[C <: Converter] (dynamic: DynamicObject, override val fieldName: Option[String] = None)
  extends DynamicDeserializerBase[C, DynamicDeserializer[C]](dynamic, fieldName) {

  protected override def createFieldDeserializer(dynamic: DynamicObject, fieldName: Option[String]): DynamicDeserializer[C]
    = new DynamicDeserializer[C](dynamic, fieldName)
}
