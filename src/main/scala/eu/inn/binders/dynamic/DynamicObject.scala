package eu.inn.binders.dynamic

import java.util.Date

class DynamicObject extends Any {

}

/*
class DynamicObject(val value: Any) /*extends Dynamic*/ {
  def isNull = value == null
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
  def apply() = new DynamicObject(DynamicType.Null, null)
  def apply(mapValue: Map[String, DynamicObject]) = new DynamicObject(DynamicType.Map, mapValue)
  def apply(seqValue: Seq[DynamicObject]) = new DynamicObject(DynamicType.Seq, seqValue)
  def apply(intValue: Int) = new DynamicObject(DynamicType.Int, intValue)
  def apply(longValue: Long) = new DynamicObject(DynamicType.Long, longValue)
  def apply(stringValue: String) = new DynamicObject(DynamicType.String, stringValue)
  def apply(floatValue: Float) = new DynamicObject(DynamicType.Float, floatValue)
  def apply(doubleValue: Double) = new DynamicObject(DynamicType.Double, doubleValue)
  def apply(booleanValue: Boolean) = new DynamicObject(DynamicType.Boolean, booleanValue)
  def apply(bigDecimalValue: BigDecimal) = new DynamicObject(DynamicType.BigDecimal, bigDecimalValue)
  def apply(dateValue: Date) = new DynamicObject(DynamicType.Date, dateValue)
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
*/