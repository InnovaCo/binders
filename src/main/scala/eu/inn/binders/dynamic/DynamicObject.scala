package eu.inn.binders.dynamic

import java.util.Date



trait DynamicObject {
}

//case object Null extends DynamicValue

case class DynamicNumber(v: BigDecimal) extends DynamicObject
case class DynamicString(v: String) extends DynamicObject
case class DynamicMap(v: Map[String, DynamicObject]) extends DynamicObject
case class DynamicSeq(v: Seq[DynamicObject]) extends DynamicObject
case class DynamicBoolean(v: Boolean) extends DynamicObject

/*
object DynamicValue {
  def apply(dynamic: DynamicValue) = if (dynamic == null) Null else dynamic
  def apply(dynamic: Option[DynamicValue]) = dynamic
}
*/

/*
class DynamicObject private(v: Any) /*extends Dynamic*/ {
  def isNull = v == null
  def asMap = v.asInstanceOf[]
  def asMapOption = if (isNull) None else Some(asMap)
  def asSeq = v.asInstanceOf[Seq[DynamicObject]]
  def asSeqOption = if (isNull) None else Some(asSeq)
  def asInt = v.asInstanceOf[BigDecimal].toIntExact
  def asIntOption = if (isNull) None else Some(asInt)
  def asLong = v.asInstanceOf[BigDecimal].toLongExact
  def asLongOption = if (isNull) None else Some(asLong)
  def asString = v.asInstanceOf[String]
  def asStringOption = if (isNull) None else Some(asString)
  def asFloat = v.asInstanceOf[BigDecimal].toFloat
  def asFloatOption = if (isNull) None else Some(asFloat)
  def asDouble = v.asInstanceOf[BigDecimal].toDouble
  def asDoubleOption = if (isNull) None else Some(asDouble)
  def asBoolean = v.asInstanceOf[Boolean]
  def asBooleanOption = if (isNull) None else Some(asBoolean)
  def asBigDecimal = v.asInstanceOf[BigDecimal]
  def asBigDecimalOption = if (isNull) None else Some(asBigDecimal)
  def asDate = new Date(v.asInstanceOf[BigDecimal].toLongExact)
  def asDateOption = if (isNull) None else Some(asDate)
  def value: Any = v

  override def toString = if (isNull) "DynamicObject(<null>)" else "DynamicObject("+v.toString+")"
  override def hashCode = if (isNull) 0 else v.hashCode()
  override def equals(other: Any) : Boolean = {

    other match {
      case null => isNull
      case d:DynamicObject => if (isNull || d.isNull) isNull && d.isNull else v.equals(d.value)
      case _ => v.equals(other)
    }
  }
}

object DynamicObject {
  def apply() = new DynamicObject(null)
  def apply(mapValue: Map[String, DynamicObject]) = new DynamicObject(mapValue)
  def apply(mapValueOption: Option[Map[String, DynamicObject]]): DynamicObject = mapValueOption.map(new DynamicObject(_)) getOrElse DynamicObject()
  def apply(seqValue: Seq[DynamicObject]) = new DynamicObject(seqValue)
  def apply(seqValueOption: Option[Seq[DynamicObject]]): DynamicObject = seqValueOption.map(new DynamicObject(_)) getOrElse DynamicObject()
  def apply(intValue: Int) = new DynamicObject(BigDecimal(intValue))
  def apply(longValue: Long) = new DynamicObject(BigDecimal(longValue))
  def apply(stringValue: String) = new DynamicObject(stringValue)
  def apply(floatValue: Float) = new DynamicObject(BigDecimal(floatValue))
  def apply(doubleValue: Double) = new DynamicObject(BigDecimal(doubleValue))
  def apply(booleanValue: Boolean) = new DynamicObject(booleanValue)
  def apply(bigDecimalValue: BigDecimal) = new DynamicObject(bigDecimalValue)
  def apply(dateValue: Date) = new DynamicObject(BigDecimal(dateValue.getTime))

/*  def unaply(d: DynamicObject): Option[Map[String, DynamicObject]] = d match { case m: Map[String, DynamicObject] => Some(m) case _ => None }
  def unaply(d: DynamicObject): Option[Seq[DynamicObject]] = d match { case s: Seq[DynamicObject] => Some(s) case _ => None }*/
}
*/
/*
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