package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object BinderProxy {

  // todo: return S ?
  def bindParameter[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (index: c.Expr[Int], obj: c.Expr[O]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindParameter[S, O](index.tree, obj.tree))
  }

  def bindClass[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (obj: c.Expr[O]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindClass[S, O](obj.tree, true))
  }

  def bindClassPartial[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (obj: c.Expr[O]): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindClass[S, O](obj.tree, false))
  }

  def bindArgs(c: Context)
              (t: c.Expr[Any]*): c.Expr[Any] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[Any](bundle.bindArgs(t.map(_.tree)))
  }

  def unbind[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbind[R, O](false, null))
  }

  def unbindPartial[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (obj: c.Expr[O]): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbind[R, O](true, obj.tree))
  }

  def unbindOne[RS: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbindOne[RS, O])
  }

  def unbindAll[RS: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context): c.Expr[O] = {

    val c0: c.type = c
    val bundle = new {
      val c: c0.type = c0
    } with BinderImplementation
    c.Expr[O](bundle.unbindAll[RS, O])
  }
}
