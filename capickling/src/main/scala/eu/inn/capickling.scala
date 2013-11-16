package eu.inn

import scala.language.implicitConversions
import scala.pickling._
import com.datastax.driver.core.ResultSet

package object capickling {
  implicit def pickleFormat = new CassandraPickleFormat
  implicit def toCassandraPickle(value: com.datastax.driver.core.Row): CassandraPickle = CassandraPickle(Some(value))
  //implicit def toBoundStatementOutput(implicit context: BoundStatementContext, in: com.datastax.driver.core.BoundStatement) : BoundStatementOutput = new BoundStatementOutput(context, in)

  implicit object DatePicklerUnpickler extends SPickler[java.util.Date] with Unpickler[java.util.Date] {
    val format = null // not used
    def pickle(picklee: java.util.Date, builder: PBuilder): Unit = builder.asInstanceOf[CassandraPickleBuilder].pickleDate(picklee)
    def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = reader.asInstanceOf[CassandraPickleReader].unpickleDate()
  }

  class SetPicklerUnpickler[T](c: Class[T]) extends SPickler[Set[T]] with Unpickler[Set[T]] {
    val format = null // not used
    def pickle(picklee: Set[T], builder: PBuilder): Unit = builder.asInstanceOf[CassandraPickleBuilder].pickleSet(picklee)
    def unpickle(tag: => FastTypeTag[_], reader: PReader): Any = reader.asInstanceOf[CassandraPickleReader].unpickleSet[T](c)
  }

  implicit object IntSetPicklerUnpickler extends SetPicklerUnpickler[Int]( classOf[Int] ) {}
  implicit object LongSetPicklerUnpickler extends SetPicklerUnpickler[Long]( classOf[Long] ) {}
  implicit object StringSetPicklerUnpickler extends SetPicklerUnpickler[String]( classOf[String] ) {}
  implicit object BooleanSetPicklerUnpickler extends SetPicklerUnpickler[Boolean]( classOf[Boolean] ) {}
  implicit object DoubleSetPicklerUnpickler extends SetPicklerUnpickler[Double]( classOf[Double] ) {}
  implicit object FloatSetPicklerUnpickler extends SetPicklerUnpickler[Float]( classOf[Float] ) {}
  implicit object DateSetPicklerUnpickler extends SetPicklerUnpickler[java.util.Date]( classOf[java.util.Date] ) {}

  implicit class ResultSetUnpickleOps[T <: ResultSet](resultSet: T) {

    def unpickleAll[OUT: Unpickler: FastTypeTag] : Iterator[OUT] = {
      val iterator = resultSet.iterator()
      import scala.collection.JavaConversions._
      iterator.map { row => row.unpickle[OUT] }
    }

    def unpickleOne[OUT: Unpickler: FastTypeTag] : Option[OUT] = {
      if (!resultSet.isExhausted)
        Some(resultSet.one.unpickle[OUT])
      else
        None
    }
  }
}