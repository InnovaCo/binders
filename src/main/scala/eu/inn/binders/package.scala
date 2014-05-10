package eu.inn

import eu.inn.binders.core.{Statement, Rows, Row}

package object binders {

  import eu.inn.internal.BinderProxy
  import language.experimental.macros

  implicit class StatementBindOps[S <: Statement[_]](val stmt: S) {
    def bindParameter[O](index: Int, obj: O) = macro BinderProxy.bindParameter[S, O]

    def bind[O](obj: O) = macro BinderProxy.bind[S, O]

    def bindPartial[O](obj: O) = macro BinderProxy.bindPartial[S, O]

    def bindArgs(t: Any*) = macro BinderProxy.bindArgs
  }

  implicit class RowUnbindOps[R <: Row](val row: R) {
    def unbind[O]: O = macro BinderProxy.unbind[R, O]

    def unbindPartial[O](obj: O): O = macro BinderProxy.unbindPartial[R, O]
  }

  implicit class RowsUnbindOps[RS <: Rows[_]](val rows: RS) {
    def unbindOne[O]: Option[O] = macro BinderProxy.unbindOne[RS, O]

    def unbindAll[O]: Iterator[O] = macro BinderProxy.unbindAll[RS, O]
  }

}
