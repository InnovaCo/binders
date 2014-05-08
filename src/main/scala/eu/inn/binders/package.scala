package eu.inn

import eu.inn.binders.core.{Statement, Rows, Row, Query}

package object binders {

  import eu.inn.internal.BinderProxy
  import language.experimental.macros

  implicit class RowUnbindOps[R <: Row](val row: R) {
    def unbind[O]: O = macro BinderProxy.unbind[R, O]

    def unbindPartial[O](obj: O): O = macro BinderProxy.unbindPartial[R, O]
  }

  implicit class StatementBindOps[S <: Statement](val stmt: S) {
    def bind[O](index: Int, obj: O) = macro BinderProxy.bind[S, O]

    def bindPartial[O](index: Int, obj: O) = macro BinderProxy.bindPartial[S, O]
  }

  implicit class RowsUnbindOps[RS <: Rows[_]](val rows: RS) {
    def unbindOne[O]: Option[O] = macro BinderProxy.unbindOne[RS, O]

    def unbindAll[O]: Iterator[O] = macro BinderProxy.unbindAll[RS, O]
  }

  implicit class QueryOps[Q <: Query[_, _]](val query: Q) {
    type rowsType = query.rowsType
    type statementType = query.statementType

    def execute(t: Any*): query.rowsType = macro BinderProxy.execute[rowsType]

    def executeWithPartialBind(t: Any*): query.rowsType = macro BinderProxy.executeWithPartialBind[rowsType]
  }

}
