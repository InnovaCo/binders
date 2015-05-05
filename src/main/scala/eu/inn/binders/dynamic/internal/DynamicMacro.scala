package eu.inn.binders.dynamic.internal


import eu.inn.binders.dynamic.Value

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

private [dynamic] object DynamicMacro {
  def fromDynamic[O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[O](bundle.fromDynamic[O])
  }

  def toDynamic[O: c.WeakTypeTag]
  (c: Context): c.Expr[Value] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[Value](bundle.toDynamic[O])
  }

  def selectDynamic[O: c.WeakTypeTag]
  (c: Context)(name: c.Expr[String]): c.Expr[O] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[O](bundle.selectDynamic[O](name))
  }
}
