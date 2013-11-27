package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object  BinderProxy {

  import scala.reflect.runtime.{universe => ru}

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag]
    (c: Context)
    (index: c.Expr[Int], obj: c.Expr[O]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[Unit](bundle.bind[S,O](index.tree, obj.tree, true))
  }

  def bindPartial[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (index: c.Expr[Int], obj: c.Expr[O]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[Unit](bundle.bind[S,O](index.tree, obj.tree, false))
  }

  def unbind[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.unbind[R,O](false, null))
  }

  def unbindPartial[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (obj: c.Expr[O]) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.unbind[R,O](true, obj.tree))
  }

  def unbindOne[RS: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.unbindOne[RS,O])
  }

  def unbindAll[RS: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.unbindAll[RS,O])
  }

  def execute[RS: c.WeakTypeTag] (c: Context)
  (t: c.Expr[Any]*): c.Expr[RS] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[RS](bundle.execute(t.map(_.tree), false))
  }

  def executeWithPartialBind[RS: c.WeakTypeTag] (c: Context)
                                 (t: c.Expr[Any]*): c.Expr[RS] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[RS](bundle.execute(t.map(_.tree), true))
  }
}
