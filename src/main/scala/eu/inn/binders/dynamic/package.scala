package eu.inn.binders

import eu.inn.binders.dynamic.internal.DynamicMacro

import scala.language.experimental.macros

package object dynamic {
  implicit class DynamicReader(val dynamic: DynamicValue) {
    def fromDynamic[O]: O = macro DynamicMacro.fromDynamic[O]
  }

  implicit class DynamicGenerator[O](val obj: O) {
    def toDynamic: DynamicValue = macro DynamicMacro.toDynamic[O]
  }
}
