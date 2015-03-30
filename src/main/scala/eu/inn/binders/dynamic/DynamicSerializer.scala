package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Serializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicSerializerBase[C <: Converter, F <: Serializer[C]] extends Serializer[C]{
  var dynamic: DynamicObject = null
  var map: scala.collection.mutable.Map[String, DynamicObject] = null
  var seq: scala.collection.mutable.Seq[DynamicObject] = null

  def getFieldSerializer(fieldName: String): Option[F] = {
    jsonGenerator.writeFieldName(fieldName)
    Some(createFieldSerializer())
  }

  protected def createFieldSerializer(): F = ???

  def writeNull(): Unit = dynamic = DynamicObject()
  def writeInteger(value: Int): Unit = dynamic = DynamicObject(value)
  def writeLong(value: Long): Unit = dynamic = DynamicObject(value)
  def writeString(value: String): Unit = dynamic = DynamicObject(value)
  def writeFloat(value: Float): Unit = dynamic = DynamicObject(value)
  def writeDouble(value: Double): Unit = dynamic = DynamicObject(value)
  def writeBoolean(value: Boolean): Unit = dynamic = DynamicObject(value)
  def writeBigDecimal(value: BigDecimal): Unit = dynamic = DynamicObject(value)
  def writeDate(value: Date): Unit = dynamic = DynamicObject(value)

  def beginObject(): Unit = {
    map = new scala.collection.mutable.HashMap[String, DynamicObject]()
  }

  def endObject(): Unit = {
    dynamic = DynamicObject(map.toMap)
    map = null
  }

  def beginArray(): Unit = {
    seq = new scala.collection.mutable.ArrayBuffer[DynamicObject]()
  }
  def endArray(): Unit = {
    dynamic = DynamicObject(seq)
    seq = null
  }
}

class DynamicSerializer[C <: Converter](override val jsonGenerator: JsonGenerator) extends DynamicSerializerBase[C, DynamicSerializer[C]](jsonGenerator){
  protected override def createFieldSerializer(): DynamicSerializer[C] = new DynamicSerializer[C](jsonGenerator)
}
