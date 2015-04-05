package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Serializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicSerializeException(message: String) extends RuntimeException(message)

trait DynamicSerializerBaseTrait[C <: Converter] extends Serializer[C] {
  def asDynamic: DynamicObject
}

class DynamicSerializerBase[C <: Converter, F <: DynamicSerializerBaseTrait[C]] extends DynamicSerializerBaseTrait[C]{
  var dynamic: DynamicObject = null
  var map: scala.collection.mutable.Map[String, DynamicSerializerBaseTrait[C]] = null
  var seq: scala.collection.mutable.ArrayBuffer[DynamicObject] = null

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

  def writeString(value: String) = writeDynamicObject(DynamicObject(value))
  def writeBoolean(value: Boolean) = writeDynamicObject(DynamicObject(value))
  def writeBigDecimal(value: BigDecimal) = writeDynamicObject(DynamicObject(value))
  def writeInt(value: Int) = writeDynamicObject(DynamicObject(value))
  def writeLong(value: Long) = writeDynamicObject(DynamicObject(value))
  def writeFloat(value: Float) = writeDynamicObject(DynamicObject(value))
  def writeDouble(value: Double) = writeDynamicObject(DynamicObject(value))
  def writeDate(value: Date) = writeDynamicObject(DynamicObject(value))

  def writeDynamicObject(value: DynamicObject): Unit = {
    if (seq != null)
      seq += value
    else
      dynamic = value
  }

  def beginObject(): Unit = {
    map = new scala.collection.mutable.HashMap[String, DynamicSerializerBaseTrait[C]]()
  }

  def endObject(): Unit = {
    dynamic = DynamicObject(map.toMap.map(kv => (kv._1, kv._2.asDynamic)))
    map = null
  }

  def beginArray(): Unit = {
    seq = new scala.collection.mutable.ArrayBuffer[DynamicObject]()
  }

  def endArray(): Unit = {
    dynamic = DynamicObject(seq.toSeq)
    seq = null
  }

  def asDynamic: DynamicObject = dynamic
}

class DynamicSerializer[C <: Converter] extends DynamicSerializerBase[C, DynamicSerializer[C]]{
  protected override def createFieldSerializer(): DynamicSerializer[C] = new DynamicSerializer[C]()
}
