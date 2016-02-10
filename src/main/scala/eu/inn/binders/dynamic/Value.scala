package eu.inn.binders.dynamic

import java.util.Date
import eu.inn.binders.dynamic.internal.DynamicMacro
import scala.language.experimental.macros
import scala.language.dynamics

trait Value extends Any with Dynamic {
  def accept[T](visitor: ValueVisitor[T]): T

  def asString: String = {
    accept[String](new ValueVisitor[String] {
      override def visitBool(d: Bool) = d.v.toString
      override def visitText(d: Text) = d.v
      override def visitObj(d: Obj) = d.v.map(kv => kv._1 + "->" + kv._2).mkString(",")
      override def visitNumber(d: Number) = d.v.toString()
      override def visitLst(d: Lst) = d.v.mkString(",")
      override def visitNull(): String = ""
    })
  }

  def asBoolean: Boolean = {
    accept[Boolean](new ValueVisitor[Boolean] {
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
      override def visitNull(): Boolean = false
    })
  }

  def asBigDecimal: BigDecimal = {
    accept[BigDecimal](new ValueVisitor[BigDecimal] {
      override def visitBool(d: Bool) = if(d.v) 1 else 0
      override def visitText(d: Text) = BigDecimal(d.v)
      override def visitObj(d: Obj) = castUnavailable("Obj to BigDecimal")
      override def visitNumber(d: Number) = d.v
      override def visitLst(d: Lst) = castUnavailable("Lst to BigDecimal")
      override def visitNull(): BigDecimal = 0
    })
  }

  def asInt: Int = asBigDecimal.toIntExact
  def asLong: Long = asBigDecimal.toLongExact
  def asDouble: Double = asBigDecimal.toDouble
  def asFloat: Float = asBigDecimal.toFloat
  def asDate: Date = new Date(asLong)

  def asMap: Map[String, Value] = {
    accept[Map[String, Value]](new ValueVisitor[Map[String, Value]] {
      override def visitBool(d: Bool) = castUnavailable("Bool to Map[]")
      override def visitText(d: Text) = castUnavailable("Text to Map[]")
      override def visitObj(d: Obj) = d.v
      override def visitNumber(d: Number) = castUnavailable("Number to Map[]")
      override def visitLst(d: Lst) = castUnavailable("Lst to Map[]")
      override def visitNull() = Map()
    })
  }

  def asSeq: Seq[Value] = {
    accept[Seq[Value]](new ValueVisitor[Seq[Value]] {
      override def visitBool(d: Bool) = castUnavailable("Bool to Seq[]")
      override def visitText(d: Text) = castUnavailable("Text to Seq[]")
      override def visitObj(d: Obj) = castUnavailable("Obj to Seq[]")
      override def visitNumber(d: Number) = castUnavailable("Number to Seq[]")
      override def visitLst(d: Lst) = d.v
      override def visitNull() = Seq()
    })
  }

  def isDefined: Boolean = !isNull

  def isNull: Boolean = {
    accept[Boolean](new ValueVisitor[Boolean] {
      override def visitBool(d: Bool) = false
      override def visitText(d: Text) = false
      override def visitObj(d: Obj) = false
      override def visitNumber(d: Number) = false
      override def visitLst(d: Lst) = false
      override def visitNull() = true
    })
  }

  def isEmpty: Boolean = {
    accept[Boolean](new ValueVisitor[Boolean] {
      override def visitBool(d: Bool) = false
      override def visitText(d: Text) = d.v.isEmpty
      override def visitObj(d: Obj) = d.v.isEmpty
      override def visitNumber(d: Number) = false
      override def visitLst(d: Lst) = d.v.isEmpty
      override def visitNull() = true
    })
  }

  def merge(other: Value): Value = {
    accept[Value](new ValueVisitor[Value] {
      override def visitBool(d: Bool) = other
      override def visitText(d: Text) = other
      override def visitObj(dOriginal: Obj) = {
        other.accept[Value](new ValueVisitor[Value] {
          override def visitNull(): Value = other
          override def visitBool(d: Bool): Value = other
          override def visitObj(dOther: Obj): Value = mergeFields(dOriginal,dOther)
          override def visitText(d: Text): Value = other
          override def visitNumber(d: Number): Value = other
          override def visitLst(d: Lst): Value = other
        })
      }
      override def visitNumber(d: Number) = other
      override def visitLst(d: Lst) = other
      override def visitNull() = other
    })
  }

  private def mergeFields(dOriginal: Obj, dOther: Obj): Obj = {
    Obj(dOriginal.v ++ dOther.v.map{
      case (k,v) => k -> dOriginal.v.get(k).map { original ⇒
        original.merge(v)
      }.getOrElse {
        v
      }
    })
  }

  private def castUnavailable(s: String) = throw new ClassCastException(s)

  def selectDynamic[T](name: String) = macro DynamicMacro.selectDynamic[T]
}

trait ValueVisitor[T] {
  def visitNumber(d: Number): T
  def visitText(d: Text): T
  def visitObj(d: Obj): T
  def visitLst(d: Lst): T
  def visitBool(d: Bool): T
  def visitNull(): T
}

case object Null extends Value {
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitNull()
}

case class Number(v: BigDecimal) extends AnyVal with Value {
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitNumber(this)
}

case class Text(v: String) extends AnyVal with Value {
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitText(this)
}

case class Obj(v: Map[String, Value] = Map()) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitObj(this)
}

case class Lst(v: Seq[Value] = Seq()) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitLst(this)
}

case class Bool(v: Boolean) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitBool(this)
}
