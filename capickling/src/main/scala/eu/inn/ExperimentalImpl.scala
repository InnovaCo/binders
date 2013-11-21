package eu.inn


import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros


object ExperimentalImpl {

  import scala.reflect.runtime.{universe => ru}

  def serialize[F: c.WeakTypeTag, T: c.WeakTypeTag]
    (c: Context)
    (from: c.Expr[F], to: c.Expr[T]): c.Expr[Unit] = {

    val c0: c.type = c
    val bundle = new { val c: c0.type = c0 } with Serializer
    c.Expr[Unit]((bundle.serialize[F,T](from.tree,to.tree)))
  }
}