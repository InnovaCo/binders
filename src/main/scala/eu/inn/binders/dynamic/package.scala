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
}
