package eu.inn.binders

import language.experimental.macros
import eu.inn.internal.BinderProxy

object Binder {
  def bindInto[S,O](stmt: S, index: Int, obj: O) : Unit = macro BinderProxy.bindInto[S,O]
  def createFrom[R,O](row: R) : O = macro BinderProxy.createFrom[R,O]
  def fillFrom[R <: Row,O](row: R, obj: O) : O = macro BinderProxy.fillFrom[R,O]
}
