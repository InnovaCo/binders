package com.maqdev.capickling

import com.datastax.driver.core._
import scala.pickling._

class Connection(cluster: Cluster, keyspace: String) {
  val session = cluster.connect(keyspace)

  class Statement(query: String) {
    private val preparedStmt = session.prepare(query)

    def execute(f : BoundStatement => Unit) : ResultSet = {
      val stmt = new BoundStatement(preparedStmt)
      f(stmt)
      session.execute(stmt)
    }
  }

  def execute[IN: SPickler: FastTypeTag](statement: Statement, t: Option[IN] = None) : ResultSet = {
    statement.execute(boundStatement => {
      if (t.isDefined)
        t.get.pickleTo(boundStatement)
    })
  }

  def selectOne[IN: SPickler: FastTypeTag, OUT: Unpickler: FastTypeTag](statement: Statement, in: Option[IN] = None) : OUT = {
    execute[IN](statement, in).one.unpickle[OUT]
  }

  def select[IN: SPickler: FastTypeTag, OUT: Unpickler: FastTypeTag](statement: Statement, in: Option[IN] = None) : Iterator[OUT] = {
    val iterator = execute[IN](statement, in).iterator()
    import scala.collection.JavaConversions._
    iterator.map { row => row.unpickle[OUT] }
  }
}

/*
TODO:
move methods into statement
make outside usage test
 */
