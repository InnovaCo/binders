package eu.inn.binders

import language.experimental.macros
import eu.inn.internal.BinderProxy

object Binder {
  def bindAllFields[S,O](stmt: S, index: Int, obj: O) : Unit = macro BinderProxy.bindAllFields[S,O]
  def bindExistingFields[S <: Statement,O](stmt: S, index: Int, obj: O) : Unit = macro BinderProxy.bindExistingFields[S,O]
  def createFrom[R,O](row: R) : O = macro BinderProxy.createFrom[R,O]
  def fillFrom[R <: Row,O](row: R, obj: O) : O = macro BinderProxy.fillFrom[R,O]
}
