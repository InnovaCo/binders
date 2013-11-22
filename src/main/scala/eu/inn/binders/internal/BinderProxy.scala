package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object BinderProxy {

  import scala.reflect.runtime.{universe => ru}

  def bindAllFields[S: c.WeakTypeTag, O: c.WeakTypeTag]
    (c: Context)
    (stmt: c.Expr[S], index: c.Expr[Int], obj: c.Expr[O]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[Unit](bundle.bind[S,O](stmt.tree, index.tree, obj.tree, true))
  }

  def bindExistingFields[S: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (stmt: c.Expr[S], index: c.Expr[Int], obj: c.Expr[O]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[Unit](bundle.bind[S,O](stmt.tree, index.tree, obj.tree, false))
  }

  def createFrom[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (row: c.Expr[R]) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.createFrom[R,O](row.tree))
  }

  def fillFrom[R: c.WeakTypeTag, O: c.WeakTypeTag]
  (c: Context)
  (row: c.Expr[R], obj: c.Expr[O]) : c.Expr[O] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with BinderImplementation
    c.Expr[O](bundle.fillFrom[R,O](row.tree, obj.tree))
  }
}
