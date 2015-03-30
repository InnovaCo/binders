package eu.inn.binders.dynamic.internal

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

object DynamicMacro {
  def parseJson[O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[O](bundle.parseJson[O])
  }

  def toJson[O: c.WeakTypeTag]
  (c: Context): c.Expr[String] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[String](bundle.toJson[O])
  }

  def writeMap[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (value: c.Expr[Map[String, O]]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[Any](bundle.writeMap[S,O](value.tree))
  }

  def readMap[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)(): c.Expr[Map[String, O]] = {
    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with DynamicMacroImpl
    c.Expr[Map[String, O]](bundle.readMap[S,O])
  }
}
