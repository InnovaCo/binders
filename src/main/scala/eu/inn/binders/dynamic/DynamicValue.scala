package eu.inn.binders.dynamic

import java.util.Date
import eu.inn.binders.dynamic.internal.DynamicMacro
import scala.language.experimental.macros
import scala.language.dynamics

trait DynamicValue extends Dynamic {
  def accept[T](visitor: DynamicVisitor[T]): T

  def asString: String = {
    accept[String](new DynamicVisitor[String] {
      override def visitBool(d: Bool) = d.v.toString
      override def visitText(d: Text) = d.v
      override def visitObj(d: Obj) = d.v.map(kv => kv._1 + "->" + kv._2).mkString(",")
      override def visitNumber(d: Number) = d.v.toString()
      override def visitLst(d: Lst) = d.v.mkString(",")
    })
  }

  def asBoolean: Boolean = {
    accept[Boolean](new DynamicVisitor[Boolean] {
      override def visitBool(d: Bool) = d.v
      override def visitText(d: Text) = d.v.toLowerCase match {
        case "true" => true
        case "y" => true
        case "yes" => true
        case "on" => true
        case "1" => true
        case "false" => false
        case "n" => false
        case "no" => false
        case "off" => false
        case "0" => false
        case _ => castUnavailable(s"String(${d.v}) to Boolean")
      }
      override def visitObj(d: Obj) = castUnavailable("Obj to Boolean")
      override def visitNumber(d: Number) = d.v != BigDecimal(0)
      override def visitLst(d: Lst) = castUnavailable("Lst to Boolean")
    })
  }

  def asBigDecimal: BigDecimal = {
    accept[BigDecimal](new DynamicVisitor[BigDecimal] {
      override def visitBool(d: Bool) = if(d.v) 1 else 0
      override def visitText(d: Text) = BigDecimal(d.v)
      override def visitObj(d: Obj) = castUnavailable("Obj to BigDecimal")
      override def visitNumber(d: Number) = d.v
      override def visitLst(d: Lst) = castUnavailable("Lst to BigDecimal")
    })
  }

  def asInt: Int = asBigDecimal.toIntExact
  def asLong: Long = asBigDecimal.toLongExact
  def asDouble: Double = asBigDecimal.toDouble
  def asFloat: Float = asBigDecimal.toFloat
  def asDate: Date = new Date(asLong)

  def asMap: Map[String, DynamicValue] = {
    accept[Map[String, DynamicValue]](new DynamicVisitor[Map[String, DynamicValue]] {
      override def visitBool(d: Bool) = castUnavailable("Bool to Map[]")
      override def visitText(d: Text) = castUnavailable("Text to Map[]")
      override def visitObj(d: Obj) = d.v
      override def visitNumber(d: Number) = castUnavailable("Number to Map[]")
      override def visitLst(d: Lst) = castUnavailable("Lst to Map[]")
    })
  }

  def asSeq: Seq[DynamicValue] = {
    accept[Seq[DynamicValue]](new DynamicVisitor[Seq[DynamicValue]] {
      override def visitBool(d: Bool) = castUnavailable("Bool to Seq[]")
      override def visitText(d: Text) = castUnavailable("Text to Seq[]")
      override def visitObj(d: Obj) = castUnavailable("Obj to Seq[]")
      override def visitNumber(d: Number) = castUnavailable("Number to Seq[]")
      override def visitLst(d: Lst) = d.v
    })
  }

  def castUnavailable(s: String) = throw new ClassCastException(s)

  def selectDynamic[T](name: String) = macro DynamicMacro.selectDynamic[T]
}

trait DynamicVisitor[T] {
  def visitNumber(d: Number): T
  def visitText(d: Text): T
  def visitObj(d: Obj): T
  def visitLst(d: Lst): T
  def visitBool(d: Bool): T
//  def visitNull()
}

/*case object Null extends DynamicValue {
  override def accept(visitor: DynamicVisitor): Unit = visitor.visitNull()
}*/

case class Number(v: BigDecimal) extends DynamicValue {
  override def accept[T](visitor: DynamicVisitor[T]): T = visitor.visitNumber(this)
}

case class Text(v: String) extends DynamicValue {
  override def accept[T](visitor: DynamicVisitor[T]): T = visitor.visitText(this)
}

case class Obj(v: Map[String, DynamicValue]) extends DynamicValue{
  override def accept[T](visitor: DynamicVisitor[T]): T = visitor.visitObj(this)
}

case class Lst(v: Seq[DynamicValue]) extends DynamicValue{
  override def accept[T](visitor: DynamicVisitor[T]): T = visitor.visitLst(this)
}

case class Bool(v: Boolean) extends DynamicValue{
  override def accept[T](visitor: DynamicVisitor[T]): T = visitor.visitBool(this)
}

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