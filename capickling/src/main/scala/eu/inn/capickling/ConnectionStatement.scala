package eu.inn.capickling

import com.datastax.driver.core._
import scala.pickling._

class ConnectionStatement(val session: Session, val queryString: String) {
  val preparedStatement = session.prepare(queryString)

  private def e(f : BoundStatementOutput => Unit) : ResultSet = {
    val stmt = new BoundStatement(preparedStatement)
    val bso = new BoundStatementOutput(stmt)
    f(bso)
    session.execute(stmt)
  }

  def query : ResultSet = e(bs=>{})

  def query[IN: SPickler: FastTypeTag](t: IN) : ResultSet = {
    e(bso => {
      t.pickleTo(bso)
    })
  }

  def query[IN0: SPickler: FastTypeTag, IN1: SPickler: FastTypeTag](t0: IN0, t1: IN1) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
    })
  }

  def query[
    IN0: SPickler: FastTypeTag,
    IN1: SPickler: FastTypeTag,
    IN2: SPickler: FastTypeTag](t0: IN0, t1: IN1, t2: IN1) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
      t2.pickleTo(bso)
    })
  }

  def query[
  IN0: SPickler: FastTypeTag,
  IN1: SPickler: FastTypeTag,
  IN2: SPickler: FastTypeTag,
  IN3: SPickler: FastTypeTag](t0: IN0, t1: IN1, t2: IN1, t3: IN3) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
      t2.pickleTo(bso)
      t3.pickleTo(bso)
    })
  }

  def query[
  IN0: SPickler: FastTypeTag,
  IN1: SPickler: FastTypeTag,
  IN2: SPickler: FastTypeTag,
  IN3: SPickler: FastTypeTag,
  IN4: SPickler: FastTypeTag](t0: IN0, t1: IN1, t2: IN1, t3: IN3, t4: IN4) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
      t2.pickleTo(bso)
      t3.pickleTo(bso)
      t4.pickleTo(bso)
    })
  }

  def query[
  IN0: SPickler: FastTypeTag,
  IN1: SPickler: FastTypeTag,
  IN2: SPickler: FastTypeTag,
  IN3: SPickler: FastTypeTag,
  IN4: SPickler: FastTypeTag,
  IN5: SPickler: FastTypeTag](t0: IN0, t1: IN1, t2: IN1, t3: IN3, t4: IN4, t5: IN5) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
      t2.pickleTo(bso)
      t3.pickleTo(bso)
      t4.pickleTo(bso)
      t5.pickleTo(bso)
    })
  }

  def query[
  IN0: SPickler: FastTypeTag,
  IN1: SPickler: FastTypeTag,
  IN2: SPickler: FastTypeTag,
  IN3: SPickler: FastTypeTag,
  IN4: SPickler: FastTypeTag,
  IN5: SPickler: FastTypeTag,
  IN6: SPickler: FastTypeTag](t0: IN0, t1: IN1, t2: IN1, t3: IN3, t4: IN4, t5: IN5, t6: IN6) : ResultSet = {
    e(bso => {
      t0.pickleTo(bso)
      t1.pickleTo(bso)
      t2.pickleTo(bso)
      t3.pickleTo(bso)
      t4.pickleTo(bso)
      t5.pickleTo(bso)
      t6.pickleTo(bso)
    })
  }
}
