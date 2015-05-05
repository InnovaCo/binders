package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Serializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class ValueSerializeException(message: String) extends RuntimeException(message)

trait ValueSerializerBaseTrait[C <: Converter] extends Serializer[C] {
  def asValue: Value
}

class ValueSerializerBase[C <: Converter, F <: ValueSerializerBaseTrait[C]] extends ValueSerializerBaseTrait[C]{
  var value: Value = null
  var map: scala.collection.mutable.Map[String, ValueSerializerBaseTrait[C]] = null
  var seq: scala.collection.mutable.ArrayBuffer[Value] = null

  def getFieldSerializer(fieldName: String): Option[F] = {
    if (map == null) {
      throw new ValueSerializeException("Can't get field serializer for nonmap: "+ fieldName)
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
  def writeDate(value: Date) = writeDynamicObject(Number(value.getTime))

  def writeDynamicObject(value: Value): Unit = {
    if (seq != null)
      seq += value
    else
      this.value = value
  }

  def beginObject(): Unit = {
    map = new scala.collection.mutable.HashMap[String, ValueSerializerBaseTrait[C]]()
  }

  def endObject(): Unit = {
    value = Obj(map.toMap.map(kv => (kv._1, kv._2.asValue)))
    map = null
  }

  def beginArray(): Unit = {
    seq = new scala.collection.mutable.ArrayBuffer[Value]()
  }

  def endArray(): Unit = {
    value = Lst(seq)
    seq = null
  }

  def asValue: Value = value
}

class ValueSerializer[C <: Converter] extends ValueSerializerBase[C, ValueSerializer[C]]{
  protected override def createFieldSerializer(): ValueSerializer[C] = new ValueSerializer[C]()
}
