package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object BinderProxy {

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (value: c.Expr[O]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bind[S, O](value.tree))
  }

  def bindArgs(c: Context)
              (t: c.Expr[Any]*): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindArgs(t.map(_.tree)))
  }

  def bindPartial[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (value: c.Expr[O]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindProduct[S, O](value.tree, true))
  }

  def unbind[D: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbind[D, O](false, null))
  }

  def unbindPartial[D: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (originalValue: c.Expr[O]): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbind[D, O](true, originalValue.tree))
  }
}
