package eu.inn.binders.dynamic

import java.util.Date

import eu.inn.binders.core.Deserializer
import eu.inn.binders.naming.Converter
import scala.language.experimental.macros

class DynamicDeserializeException(message: String) extends RuntimeException(message)

class DynamicDeserializerBase[C <: Converter, I <: Deserializer[C]] (dynamic: Any, val fieldName: Option[String])
  extends Deserializer[C] {

  def iterator(): Iterator[I] = {
    dynamic match {
      case s:Seq[_] => s.toIterator.map(createFieldDeserializer(_, None))
      case m:Map[String,_] => m.toIterator.map(kv => createFieldDeserializer(kv._2, Some(kv._1)))
      case _ => throw new DynamicDeserializeException("Couldn't iterate on: " + dynamic)
    }
  }

  protected def createFieldDeserializer(dynamic: Any, fieldName: Option[String]): I = ??? //new JsonDeserializer[C](jsonNode, fieldName)
  
  def isNull: Boolean = dynamic == null // todo: None?
  def readString(): String = dynamic.asInstanceOf[String]
  def readInt(): Int = dynamic.asInstanceOf[Int]
  def readLong(): Long = dynamic.asInstanceOf[Long]
  def readDouble(): Double = dynamic.asInstanceOf[Double]
  def readFloat(): Float = dynamic.asInstanceOf[Float]
  def readBoolean(): Boolean = dynamic.asInstanceOf[Boolean]
  def readBigDecimal(): BigDecimal = dynamic.asInstanceOf[BigDecimal]
  def readDate(): Date = dynamic.asInstanceOf[Date]
//  def readMap(): Map[String,DynamicObject] = dynamic.asMap
//  def readSeq(): Seq[DynamicObject] = dynamic.asSeq
}

class DynamicDeserializer[C <: Converter] (dynamic: Any, override val fieldName: Option[String] = None)
  extends DynamicDeserializerBase[C, DynamicDeserializer[C]](dynamic, fieldName) {

  protected override def createFieldDeserializer(dynamic: Any, fieldName: Option[String]): DynamicDeserializer[C]
    = new DynamicDeserializer[C](dynamic, fieldName)
}
