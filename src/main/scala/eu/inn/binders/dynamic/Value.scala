package eu.inn.binders.dynamic

import java.util.Date
import eu.inn.binders.dynamic.internal.DynamicMacro
import scala.language.experimental.macros
import scala.language.dynamics

trait Value extends Any with Dynamic {
  def accept[T](visitor: ValueVisitor[T]): T

  def asString: String = {
    accept[String](Visitors.asStringVisitor)
  }

  def asBoolean: Boolean = {
    accept[Boolean](Visitors.asBooleanVisitor)
  }

  def asBigDecimal: BigDecimal = {
    accept[BigDecimal](Visitors.asBigDecimalVisitor)
  }

  def asInt: Int = asBigDecimal.toIntExact
  def asLong: Long = asBigDecimal.toLongExact
  def asDouble: Double = asBigDecimal.toDouble
  def asFloat: Float = asBigDecimal.toFloat
  def asDate: Date = new Date(asLong)

  def asMap: Map[String, Value] = {
    accept[Map[String, Value]](Visitors.asMapVisitor)
  }

  def asSeq: Seq[Value] = {
    accept[Seq[Value]](Visitors.asSeqVisitor)
  }

  def isDefined: Boolean = !isNull

  def isNull: Boolean = {
    accept[Boolean](Visitors.isNullVisitor)
  }

  def isEmpty: Boolean = {
    accept[Boolean](Visitors.isEmptyVisitor)
  }

  def merge(other: Value): Value = other

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

case class Obj(v: Map[String, Value]) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitObj(this)

  override def merge(other: Value): Value = {
    other match {
      case o: Obj ⇒
        Obj(v ++ o.v.map {
          case (k, otherV) => k -> v.get(k).map { originalV ⇒
            originalV.merge(otherV)
          }.getOrElse {
            otherV
          }
        })
      case _ ⇒
        super.merge(other)
    }
  }
}

object Obj {
  def apply(v: (String,Value)*): Obj = new Obj(v.toMap)
  def apply(): Obj = new Obj(Map.empty)
}

case class Lst(v: Seq[Value]) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitLst(this)
}

object LstV { // can't be Lst :-(
  def apply(seq: Value*): Lst = Lst(seq)
}

case class Bool(v: Boolean) extends AnyVal with Value{
  override def accept[T](visitor: ValueVisitor[T]): T = visitor.visitBool(this)
}

private [dynamic] object Visitors {
  val asStringVisitor = new ValueVisitor[String] {
    override def visitBool(d: Bool) = d.v.toString
    override def visitText(d: Text) = d.v
    override def visitObj(d: Obj) = d.v.map(kv => kv._1 + "->" + kv._2).mkString(",")
    override def visitNumber(d: Number) = d.v.toString()
    override def visitLst(d: Lst) = d.v.mkString(",")
    override def visitNull(): String = ""
  }

  val asBooleanVisitor = new ValueVisitor[Boolean] {
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
  }

  val asBigDecimalVisitor = new ValueVisitor[BigDecimal] {
    override def visitBool(d: Bool) = if(d.v) 1 else 0
    override def visitText(d: Text) = BigDecimal(d.v)
    override def visitObj(d: Obj) = castUnavailable("Obj to BigDecimal")
    override def visitNumber(d: Number) = d.v
    override def visitLst(d: Lst) = castUnavailable("Lst to BigDecimal")
    override def visitNull(): BigDecimal = 0
  }

  val asMapVisitor = new ValueVisitor[Map[String, Value]] {
    override def visitBool(d: Bool) = castUnavailable("Bool to Map[]")
    override def visitText(d: Text) = castUnavailable("Text to Map[]")
    override def visitObj(d: Obj) = d.v
    override def visitNumber(d: Number) = castUnavailable("Number to Map[]")
    override def visitLst(d: Lst) = castUnavailable("Lst to Map[]")
    override def visitNull() = Map()
  }

  val asSeqVisitor = new ValueVisitor[Seq[Value]] {
    override def visitBool(d: Bool) = castUnavailable("Bool to Seq[]")
    override def visitText(d: Text) = castUnavailable("Text to Seq[]")
    override def visitObj(d: Obj) = castUnavailable("Obj to Seq[]")
    override def visitNumber(d: Number) = castUnavailable("Number to Seq[]")
    override def visitLst(d: Lst) = d.v
    override def visitNull() = Seq()
  }

  val isNullVisitor = new ValueVisitor[Boolean] {
    override def visitBool(d: Bool) = false
    override def visitText(d: Text) = false
    override def visitObj(d: Obj) = false
    override def visitNumber(d: Number) = false
    override def visitLst(d: Lst) = false
    override def visitNull() = true
  }

  val isEmptyVisitor = new ValueVisitor[Boolean] {
    override def visitBool(d: Bool) = false
    override def visitText(d: Text) = d.v.isEmpty
    override def visitObj(d: Obj) = d.v.isEmpty
    override def visitNumber(d: Number) = false
    override def visitLst(d: Lst) = d.v.isEmpty
    override def visitNull() = true
  }

  def castUnavailable(s: String) = throw new ClassCastException(s)
}