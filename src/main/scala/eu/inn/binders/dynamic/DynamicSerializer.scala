package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Serializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicSerializeException(message: String) extends RuntimeException(message)

trait DynamicSerializerBaseTrait[C <: Converter] extends Serializer[C] {
  def asDynamic: DynamicValue
}

class DynamicSerializerBase[C <: Converter, F <: DynamicSerializerBaseTrait[C]] extends DynamicSerializerBaseTrait[C]{
  var dynamic: DynamicValue = null
  var map: scala.collection.mutable.Map[String, DynamicSerializerBaseTrait[C]] = null
  var seq: scala.collection.mutable.ArrayBuffer[DynamicValue] = null

  def getFieldSerializer(fieldName: String): Option[F] = {
    if (map == null) {
      throw new DynamicSerializeException("Can't get field serializer for nonmap: "+ fieldName)
    }

    val f = createFieldSerializer()
    map += fieldName -> f
    Some(f)
  }

  protected def createFieldSerializer(): F = ???

  def writeNull() = writeDynamicObject(null)

  def writeString(value: String) = writeDynamicObject(Text(value))
  def writeBoolean(value: Boolean) = writeDynamicObject(Bool(value))
  def writeBigDecimal(value: BigDecimal) = writeDynamicObject(Number(value))
  def writeInt(value: Int) = writeDynamicObject(Number(value))
  def writeLong(value: Long) = writeDynamicObject(Number(value))
  def writeFloat(value: Float) = writeDynamicObject(Number(BigDecimal(value)))
  def writeDouble(value: Double) = writeDynamicObject(Number(value))
  def writeDate(value: Date) = writeDynamicObject(Number(value.getTime()))

  def writeDynamicObject(value: DynamicValue): Unit = {
    if (seq != null)
      seq += value
    else
      dynamic = value
  }

  def beginObject(): Unit = {
    map = new scala.collection.mutable.HashMap[String, DynamicSerializerBaseTrait[C]]()
  }

  def endObject(): Unit = {
    dynamic = Obj(map.toMap.map(kv => (kv._1, kv._2.asDynamic)))
    map = null
  }

  def beginArray(): Unit = {
    seq = new scala.collection.mutable.ArrayBuffer[DynamicValue]()
  }

  def endArray(): Unit = {
    dynamic = Lst(seq)
    seq = null
  }

  def asDynamic: DynamicValue = dynamic
}

class DynamicSerializer[C <: Converter] extends DynamicSerializerBase[C, DynamicSerializer[C]]{
  protected override def createFieldSerializer(): DynamicSerializer[C] = new DynamicSerializer[C]()
}
