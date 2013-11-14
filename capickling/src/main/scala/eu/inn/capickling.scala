package eu.inn

import scala.language.implicitConversions
import scala.pickling._

package object capickling {
  implicit def pickleFormat = new CassandraPickleFormat
  implicit def toCassandraPickle(value: com.datastax.driver.core.Row): CassandraPickle = CassandraPickle(Some(value))
  implicit def toBoundStatementOutput(in: com.datastax.driver.core.BoundStatement) : BoundStatementOutput = new BoundStatementOutput(in)

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
}

import scala.pickling._
import scala.reflect.runtime.universe._
import scala.collection.mutable.Stack
import com.datastax.driver.core.{BoundStatement, Row}


case class CassandraPickle(value: Option[com.datastax.driver.core.Row]) extends Pickle {
  type ValueType = Option[Row]
  type PickleFormatType = CassandraPickleFormat
}

class CassandraPickleFormat/*(implicit out: BoundStatement)*/ extends PickleFormat {
  type PickleType = CassandraPickle
  type OutputType = Output[BoundStatement]

  def createBuilder() = throw new NotImplementedError()// new CassandraPickleBuilder(this, out)
  def createBuilder(out: Output[BoundStatement]): PBuilder = new CassandraPickleBuilder(this, out.result())
  def createReader(pickle: CassandraPickle, mirror: Mirror) = new CassandraPickleReader(pickle.value.get, mirror, this)
}

class CassandraPickleBuilder(format: CassandraPickleFormat, out: BoundStatement, useIndex: Boolean = false) extends PBuilder with PickleTools {
  import java.nio.ByteBuffer
  import scala.collection.JavaConversions._

  private var index = 0;

  private def nextIndex = {
    val prevIndex = index; index = prevIndex + 1; prevIndex
  }

  private val tags = new Stack[FastTypeTag[_]]()
  private val names = new Stack[String]()

  private def popName = names.pop

  private def outSetList[T](nextIndex: Int, picklee: Any) = out.setList[T](nextIndex, picklee.asInstanceOf[Array[T]].toList)
  private def outSetList[T](name: String, picklee: Any) = out.setList[T](name, picklee.asInstanceOf[Array[T]].toList)

  private val primitives =
    if (useIndex)
      Map[String, Any => Unit](
        FastTypeTag.Null.key -> ((picklee: Any) => nextIndex),
        FastTypeTag.Ref.key -> ((picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")),
        FastTypeTag.Int.key -> ((picklee: Any) => out.setInt(nextIndex, picklee.asInstanceOf[Int])),
        FastTypeTag.Long.key -> ((picklee: Any) => out.setLong(nextIndex, picklee.asInstanceOf[Long])),
        FastTypeTag.Short.key -> ((picklee: Any) => out.setInt(nextIndex, picklee.asInstanceOf[Short])),
        FastTypeTag.Double.key -> ((picklee: Any) => out.setDouble(nextIndex, picklee.asInstanceOf[Double])),
        FastTypeTag.Float.key -> ((picklee: Any) => out.setFloat(nextIndex, picklee.asInstanceOf[Float])),
        FastTypeTag.Boolean.key -> ((picklee: Any) => out.setBool(nextIndex, picklee.asInstanceOf[Boolean])),
        FastTypeTag.Byte.key -> ((picklee: Any) => out.setInt(nextIndex, picklee.asInstanceOf[Byte])),
        FastTypeTag.Char.key -> ((picklee: Any) => out.setString(nextIndex, picklee.asInstanceOf[Char].toString)),
        FastTypeTag.ScalaString.key -> ((picklee: Any) => out.setString(nextIndex, picklee.toString)),
        FastTypeTag.JavaString.key -> ((picklee: Any) => out.setString(nextIndex, picklee.toString)),
        FastTypeTag.ArrayByte.key -> ((picklee: Any) => outSetList[Byte](nextIndex, picklee)),
        FastTypeTag.ArrayShort.key -> ((picklee: Any) => outSetList[Short](nextIndex, picklee)),
        FastTypeTag.ArrayChar.key -> ((picklee: Any) => outSetList[Char](nextIndex, picklee)),
        FastTypeTag.ArrayInt.key -> ((picklee: Any) => outSetList[Int](nextIndex, picklee)),
        FastTypeTag.ArrayLong.key -> ((picklee: Any) => outSetList[Long](nextIndex, picklee)),
        FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => outSetList[Boolean](nextIndex, picklee)),
        FastTypeTag.ArrayFloat.key -> ((picklee: Any) => outSetList[Float](nextIndex, picklee)),
        FastTypeTag.ArrayDouble.key -> ((picklee: Any) => outSetList[Double](nextIndex, picklee))

      )
    else
      Map[String, Any => Unit](
        FastTypeTag.Null.key -> ((picklee: Any) => popName),
        FastTypeTag.Ref.key -> ((picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")),
        FastTypeTag.Int.key -> ((picklee: Any) => out.setInt(popName, picklee.asInstanceOf[Int])),
        FastTypeTag.Long.key -> ((picklee: Any) => out.setLong(popName, picklee.asInstanceOf[Long])),
        FastTypeTag.Short.key -> ((picklee: Any) => out.setInt(popName, picklee.asInstanceOf[Short])),
        FastTypeTag.Double.key -> ((picklee: Any) => out.setDouble(popName, picklee.asInstanceOf[Double])),
        FastTypeTag.Float.key -> ((picklee: Any) => out.setFloat(popName, picklee.asInstanceOf[Float])),
        FastTypeTag.Boolean.key -> ((picklee: Any) => out.setBool(popName, picklee.asInstanceOf[Boolean])),
        FastTypeTag.Byte.key -> ((picklee: Any) => out.setInt(popName, picklee.asInstanceOf[Byte])),
        FastTypeTag.Char.key -> ((picklee: Any) => out.setString(popName, picklee.asInstanceOf[Char].toString)),
        FastTypeTag.ScalaString.key -> ((picklee: Any) => out.setString(popName, picklee.toString)),
        FastTypeTag.JavaString.key -> ((picklee: Any) => out.setString(popName, picklee.toString)),
        FastTypeTag.ArrayByte.key -> ((picklee: Any) => out.setBytes(popName, ByteBuffer.wrap(picklee.asInstanceOf[Array[Byte]]))),
        FastTypeTag.ArrayShort.key -> ((picklee: Any) => out.setList[Short](popName, picklee.asInstanceOf[Array[Short]].toList)),
        FastTypeTag.ArrayChar.key -> ((picklee: Any) => out.setList[Char](popName, picklee.asInstanceOf[Array[Char]].toList)),
        FastTypeTag.ArrayInt.key -> ((picklee: Any) => out.setList[Int](popName, picklee.asInstanceOf[Array[Int]].toList)),
        FastTypeTag.ArrayLong.key -> ((picklee: Any) => out.setList[Long](popName, picklee.asInstanceOf[Array[Long]].toList)),
        FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => out.setList[Boolean](popName, picklee.asInstanceOf[Array[Boolean]].toList)),
        FastTypeTag.ArrayFloat.key -> ((picklee: Any) => out.setList[Float](popName, picklee.asInstanceOf[Array[Float]].toList)),
        FastTypeTag.ArrayDouble.key -> ((picklee: Any) => out.setList[Double](popName, picklee.asInstanceOf[Array[Double]].toList))
      )

  def beginEntry(picklee: Any): PBuilder = beginEntry(picklee, (hints: Hints) => {
    if (primitives.contains(hints.tag.key)) {
      if (hints.isElidedType) primitives(hints.tag.key)(picklee)
      else {
        primitives(hints.tag.key)(picklee)
      }
    } /*else {
      throw new IllegalArgumentException("Couldn't pickle " + hints.tag.key)
    }*/
  })

  def beginEntry(picklee: Any, pickle: Hints => Unit): PBuilder = withHints {
    hints =>
      if (hints.oid != -1) {
        throw new UnsupportedOperationException("Ref's isn't supported")
      } else {
        tags.push(hints.tag)
        pickle(hints)
      }
      this
  }

  def putField(name: String, pickler: PBuilder => Unit): PBuilder = {
    names.push(name)
    pickler(this)
    this
  }

  def endEntry(): Unit = {
    if (primitives.contains(tags.pop().key)) () // do nothing
  }

  def beginCollection(length: Int): PBuilder = throw new UnsupportedOperationException("Collections aren't supported")

  def putElement(pickler: PBuilder => Unit): PBuilder = {
    pickler(this)
    this
  }

  def endCollection(l: Int): Unit = throw new UnsupportedOperationException("Collections aren't supported")

  def result(): CassandraPickle = {
    assert(tags.isEmpty, tags)
    CassandraPickle(None)
  }

  def pickleDate(date: java.util.Date) {
    beginEntry(date, (hints: Hints) => {
      if (useIndex) {
        out.setDate(nextIndex, date)
      }
      else {
        out.setDate(popName, date)
      }
    })
    endEntry()
  }

  def pickleSet[T](set: Set[T]) {
    beginEntry(set, (hints: Hints) => {
      if (useIndex) {
        out.setSet[T](nextIndex, set)
      }
      else {
        out.setSet[T](popName, set)
      }
    })
    endEntry()
  }

}

class CassandraPickleReader(row: Row, val mirror: Mirror, format: CassandraPickleFormat, fieldName: String = "") extends PReader with PickleTools {

  import scala.collection.JavaConversions._

  private var lastReadTag: FastTypeTag[_] = null
  private val primitives = Map[String, () => Any](
    FastTypeTag.Null.key -> (() => null),
    FastTypeTag.Ref.key -> (() => throw new NotImplementedError()),//lookupUnpicklee(datum.asInstanceOf[JSONObject].obj("$ref").asInstanceOf[Double].toInt)),
    FastTypeTag.Int.key -> (() => row.getInt(fieldName)),
    FastTypeTag.Short.key -> (() => row.getInt(fieldName).toShort),
    FastTypeTag.Double.key -> (() => row.getDouble(fieldName)),
    FastTypeTag.Float.key -> (() => row.getFloat(fieldName)),
    FastTypeTag.Long.key -> (() => row.getLong(fieldName)),
    FastTypeTag.Byte.key -> (() => row.getInt(fieldName).toByte),
    FastTypeTag.Boolean.key -> (() => row.getBool(fieldName)),
    FastTypeTag.Char.key -> (() => row.getString(fieldName).charAt(0)),
    FastTypeTag.ScalaString.key -> (() => row.getString(fieldName)),
    FastTypeTag.JavaString.key -> (() => row.getString(fieldName)),
    FastTypeTag.ArrayByte.key -> (() => row.getBytes(fieldName).array()),
    FastTypeTag.ArrayShort.key -> (() => (row.getList[Short](fieldName, classOf[Short])).map(_.toShort).toArray),
    FastTypeTag.ArrayChar.key -> (() => (row.getList[Char](fieldName, classOf[Char])).map(_.toChar).toArray),
    FastTypeTag.ArrayInt.key -> (() => (row.getList[Int](fieldName, classOf[Int])).map(_.toInt).toArray),
    FastTypeTag.ArrayLong.key -> (() => (row.getList[Long](fieldName, classOf[Long])).map(_.toLong).toArray),
    FastTypeTag.ArrayBoolean.key -> (() => (row.getList[Boolean](fieldName, classOf[Boolean])).map(_ == true).toArray),
    FastTypeTag.ArrayFloat.key -> (() => (row.getList[Float](fieldName, classOf[Float])).map(_.toFloat).toArray),
    FastTypeTag.ArrayDouble.key -> (() => (row.getList[Double](fieldName, classOf[Double])).map(_.toDouble).toArray)
  )

  private def mkNestedReader(fieldName: String) = {
    val nested = new CassandraPickleReader(row, mirror, format, fieldName)
    if (this.areHintsPinned) {
      nested.areHintsPinned = true
      nested.hints = hints
      nested.lastReadTag = lastReadTag
    }
    nested
  }

  def beginEntryNoTag(): String = beginEntry().key

  def beginEntry(): FastTypeTag[_] = {
    lastReadTag = withHints { hints =>
      if (hints.isElidedType) {
        if (row.isNull(fieldName))
          FastTypeTag.Null
        else
          hints.tag
      } else
        hints.tag
    }
    lastReadTag
  }

  def atPrimitive: Boolean = primitives.contains(lastReadTag.key)

  def readPrimitive(): Any = {
    primitives(lastReadTag.key)()
  }

  def atObject: Boolean = !atPrimitive

  def readField(name: String): CassandraPickleReader = {
    mkNestedReader(name)
  }

  def endEntry(): Unit = {}

  def beginCollection(): PReader = ???

  def readLength(): Int = ???

  def readElement(): PReader = ???

  def endCollection(): Unit = {}

  def unpickleDate(): java.util.Date = row.getDate(fieldName)

  def unpickleSet[T](m: Class[T]): Set[T] = row.getSet[T](fieldName, m).toSet
}

class BoundStatementOutput(val bsoInitial: BoundStatement) extends Output[BoundStatement] {
  var bs: BoundStatement = bsoInitial
  def put(obj: com.datastax.driver.core.BoundStatement): this.type = { bs = obj; ??? }
  def result = bs
}