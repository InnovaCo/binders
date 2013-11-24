package eu.inn

package object binders {
  import eu.inn.internal.BinderProxy
  import language.experimental.macros

  implicit class RowUnbindOps[R <: Row](val row: R) {
    def unbind[O] : O = macro BinderProxy.unbind[R,O]
    def unbindPartial[O](obj: O) : O = macro BinderProxy.unbindPartial[R,O]
  }

  implicit class StatementBindOps[S <: Statement](val stmt: S) {
    def bind[O](index: Int, obj: O) = macro BinderProxy.bind[S,O]
    def bindPartial[O](index: Int, obj: O) = macro BinderProxy.bindPartial[S,O]
  }

  implicit class RowsUnbindOps[RS <: Rows[_]](val rows: RS) {
    def unbindOne[O] : Option[O] = None
  }
}
