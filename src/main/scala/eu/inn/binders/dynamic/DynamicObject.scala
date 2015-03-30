package eu.inn.binders.dynamic

import java.util.Date

case class DynamicObject(typ: DynamicType.Value, value: Any) /*extends Dynamic*/ {
  def isNull = typ == DynamicType.Null
  def asMap = value.asInstanceOf[Map[String, DynamicObject]]
  def asSeq = value.asInstanceOf[Seq[DynamicObject]]
  def asInt = value.asInstanceOf[Int]
  def asLong = value.asInstanceOf[Long]
  def asString = value.asInstanceOf[String]
  def asFloat = value.asInstanceOf[Float]
  def asDouble = value.asInstanceOf[Double]
  def asBoolean = value.asInstanceOf[Boolean]
  def asBigDecimal = value.asInstanceOf[BigDecimal]
  def asDate = value.asInstanceOf[Date]
}

object DynamicObject {
  def apply() = DynamicObject(DynamicType.Null, null)
  def apply(mapValue: Map[String, DynamicObject]) = DynamicObject(DynamicType.Map, mapValue)
  def apply(seqValue: Seq[DynamicObject]) = DynamicObject(DynamicType.Seq, seqValue)
  def apply(intValue: Int) = DynamicObject(DynamicType.Int, intValue)
  def apply(longValue: Long) = DynamicObject(DynamicType.Long, longValue)
  def apply(stringValue: String) = DynamicObject(DynamicType.String, stringValue)
  def apply(floatValue: Float) = DynamicObject(DynamicType.Float, floatValue)
  def apply(doubleValue: Double) = DynamicObject(DynamicType.Double, doubleValue)
  def apply(booleanValue: Boolean) = DynamicObject(DynamicType.Boolean, booleanValue)
  def apply(bigDecimalValue: BigDecimal) = DynamicObject(DynamicType.BigDecimal, bigDecimalValue)
  def apply(dateValue: Date) = DynamicObject(DynamicType.Date, dateValue)
}

object DynamicType extends Enumeration {
  type DynamicType = Value
  val
    Null,
    Seq,
    Map,
    Int,
    Long,
    String,
    Float,
    Double,
    Boolean,
    BigDecimal,
    Date
  = Value
}
