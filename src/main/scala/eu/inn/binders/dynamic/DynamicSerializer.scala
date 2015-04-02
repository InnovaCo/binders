package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Serializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicSerializeException(message: String) extends RuntimeException(message)

trait DynamicSerializerBaseTrait[C <: Converter] extends Serializer[C] {
  def asDynamic: Any
}

class DynamicSerializerBase[C <: Converter, F <: DynamicSerializerBaseTrait[C]] extends DynamicSerializerBaseTrait[C]{
  var dynamic: Any = null
  var map: scala.collection.mutable.Map[String, DynamicSerializerBaseTrait[C]] = null
  var seq: scala.collection.mutable.ArrayBuffer[Any] = null

  def getFieldSerializer(fieldName: String): Option[F] = {
    if (map != null) {
      throw new DynamicSerializeException("Can't get field serializer for nonmap: "+ fieldName)
    }

    val f = createFieldSerializer()
    map += fieldName -> f
    Some(f)
  }

  protected def createFieldSerializer(): F = ???

  def writeNull() = writeDynamicObject(null)
  def writeInteger(value: Int) = writeDynamicObject(value)
  def writeLong(value: Long) = writeDynamicObject(value)
  def writeString(value: String) = writeDynamicObject(value)
  def writeFloat(value: Float) = writeDynamicObject(value)
  def writeDouble(value: Double) = writeDynamicObject(value)
  def writeBoolean(value: Boolean) = writeDynamicObject(value)
  def writeBigDecimal(value: BigDecimal) = writeDynamicObject(value)
  def writeDate(value: Date) = writeDynamicObject(value)
  def writeDynamicObject(value: Any): Unit = {
    if (seq != null)
      seq += value
    else
      dynamic = value
  }

  def beginObject(): Unit = {
    map = new scala.collection.mutable.HashMap[String, DynamicSerializerBaseTrait[C]]()
  }

  def endObject(): Unit = {
    dynamic = map.toMap.map(kv => (kv._1, kv._2.asDynamic))
    map = null
  }

  def beginArray(): Unit = {
    seq = new scala.collection.mutable.ArrayBuffer[Any]()
  }

  def endArray(): Unit = {
    dynamic = seq
    seq = null
  }

  def asDynamic: Any = dynamic
}

class DynamicSerializer[C <: Converter] extends DynamicSerializerBase[C, DynamicSerializer[C]]{
  protected override def createFieldSerializer(): DynamicSerializer[C] = new DynamicSerializer[C]()
}
