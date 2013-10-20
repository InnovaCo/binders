package com.maqdev.capickling

import com.datastax.driver.core._
import scala.pickling._

class ConnectionStatement(session: Session, query: String) {
  val preparedStatement = session.prepare(query)

  private def e(f : BoundStatement => Unit) : ResultSet = {
    val stmt = new BoundStatement(preparedStatement)
    f(stmt)
    session.execute(stmt)
  }

  def execute : ResultSet = e(bs=>{})

  def executeWith[IN: SPickler: FastTypeTag](t: IN) : ResultSet = {
    e(boundStatement => {
        t.pickleTo(boundStatement)
    })
  }

  def selectOne[OUT: Unpickler: FastTypeTag] : Option[OUT] = {
    val result = execute
    if (!result.isExhausted)
      Some(result.one.unpickle[OUT])
    else
      None
  }

  def select[OUT: Unpickler: FastTypeTag] : Iterator[OUT] = {
    val iterator = execute.iterator()
    import scala.collection.JavaConversions._
    iterator.map { row => row.unpickle[OUT] }
  }

  def selectOneWith[IN: SPickler: FastTypeTag, OUT: Unpickler: FastTypeTag](in: IN) : Option[OUT] = {
    val result = executeWith[IN](in)
    if (!result.isExhausted)
      Some(result.one.unpickle[OUT])
    else
      None
  }

  def selectWith[IN: SPickler: FastTypeTag, OUT: Unpickler: FastTypeTag](in: IN) : Iterator[OUT] = {
    val iterator = executeWith[IN](in).iterator()
    import scala.collection.JavaConversions._
    iterator.map { row => row.unpickle[OUT] }
  }
}
