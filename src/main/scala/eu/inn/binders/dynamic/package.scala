package eu.inn.binders

import eu.inn.binders.dynamic.internal.DynamicMacro

import scala.language.experimental.macros

package object dynamic {
  implicit class ValueReader(val value: Value) {
    def fromDynamic[O]: O = macro DynamicMacro.fromDynamic[O]
  }

  implicit class ValueGenerator[O](val obj: O) {
    def toDynamic: Value = macro DynamicMacro.toDynamic[O]
  }

  implicit def int2number(i: Int): Number = Number(i)
  implicit def long2number(i: Long): Number = Number(i)
  implicit def bigdecimal2number(i: BigDecimal): Number = Number(i)
  implicit def double2number(i: Double): Number = Number(i)

  implicit def string2text(s: String): Text = Text(s)
  implicit def boolean2bool(b: Boolean): Bool = Bool(b)

  implicit def seq2lst(seq: Seq[Value]): Lst = Lst(seq)
  implicit def map2obj(map: Map[String, Value]): Obj = Obj(map)
}
