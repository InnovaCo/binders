package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object BinderImpl {

  import scala.reflect.runtime.{universe => ru}

  def bindInto[F: c.WeakTypeTag, T: c.WeakTypeTag]
    (c: Context)
    (from: c.Expr[F], to: c.Expr[T]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with DbPicklingSerializer
    c.Expr[Unit]((bundle.serialize[F,T](from.tree,to.tree)))
  }
}