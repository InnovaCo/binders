package eu.inn.binders.dynamic

import eu.inn.binders.core.{Deserializer, Serializer}
import eu.inn.binders.naming.{PlainConverter, Converter}

trait DynamicSerializerFactory[C <: Converter, S <: DynamicSerializerBase[C,_], D <: Deserializer[C]] {
  def withDeserializer[T](dynamic: DynamicValue, codeBlock: D ⇒ T): T = {
    val deserializer = createDeserializer(dynamic)
    codeBlock(deserializer)
  }

  def withSerializer(codeBlock: S ⇒ Unit): DynamicValue = {
    val serializer = createSerializer()
    codeBlock(serializer)
    serializer.asDynamic
  }

  def createSerializer(): S
  def createDeserializer(dynamic: DynamicValue): D
}

class DefaultDynamicSerializerFactory[C <: Converter] extends DynamicSerializerFactory[C, DynamicSerializer[C], DynamicDeserializer[C]] {
  override def createSerializer(): DynamicSerializer[C] = new DynamicSerializer[C]()
  override def createDeserializer(dynamic: DynamicValue): DynamicDeserializer[C] = new DynamicDeserializer[C](dynamic)
}

object DynamicSerializerFactory {
  implicit val defaultSerializerFactory = new DefaultDynamicSerializerFactory[PlainConverter]
  def findFactory[C <: Converter, S <: DynamicSerializerBase[C,_], D <: Deserializer[C]]()
    (implicit factory: DynamicSerializerFactory[C, S, D]): DynamicSerializerFactory[C, S, D] = factory
}
