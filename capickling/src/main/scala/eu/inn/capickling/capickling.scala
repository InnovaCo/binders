package eu.inn.capickling

import scala.pickling._
import scala.reflect.runtime.universe._
import scala.collection.mutable.Stack
import com.datastax.driver.core.{BoundStatement, Row}
import scala.reflect.runtime._

case class CassandraPickle(value: Option[com.datastax.driver.core.Row]) extends Pickle {
  type ValueType = Option[Row]
  type PickleFormatType = CassandraPickleFormat
}

class CassandraPickleFormat/*(implicit out: BoundStatement)*/ extends PickleFormat {
  type PickleType = CassandraPickle
  type OutputType = Output[BoundStatement]

  def createBuilder() = throw new NotImplementedError()// new CassandraPickleBuilder(this, out)
  def createBuilder(out: Output[BoundStatement]): PBuilder = new CassandraPickleBuilder(this, out.asInstanceOf[BoundStatementOutput])
  def createReader(pickle: CassandraPickle, mirror: Mirror) = new CassandraPickleReader(pickle.value.get, mirror, this)
}

class CassandraPickleBuilder(format: CassandraPickleFormat, out: BoundStatementOutput, initialNextIndex: Integer = 0) extends PBuilder with PickleTools {
  import java.nio.ByteBuffer
  import scala.collection.JavaConversions._

  private val boundStatement: BoundStatement = out.result
  private var index: Int = out.nextIndex;
  private var levelSome: Int = 0

  private def nextIndex = {
    val prevIndex = index
    index = prevIndex + 1
    out.nextIndex = index
    prevIndex
  }

  private val tags = new Stack[FastTypeTag[_]]()
  private val names = new Stack[String]()

  private def popName = names.pop

  private def outSetList[T](nextIndex: Int, picklee: Any) = boundStatement.setList[T](nextIndex, picklee.asInstanceOf[Array[T]].toList)
  private def outSetList[T](name: String, picklee: Any) = boundStatement.setList[T](name, picklee.asInstanceOf[Array[T]].toList)
  private def useIndex = names.isEmpty

  private val primitives =
      Map[String, (Any => Any, Any => Any)](
        "scala.None.type" -> (
          (picklee: Any) => nextIndex,
          (picklee: Any) => popName
        ),
        FastTypeTag.Null.key -> (
          (picklee: Any) => nextIndex,
          (picklee: Any) => popName
        ),
        FastTypeTag.Ref.key -> (
          (picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly"),
          (picklee: Any) => throw new Error("fatal error: shouldn't be invoked explicitly")
        ),
        FastTypeTag.Int.key -> (
          (picklee: Any) => boundStatement.setInt(nextIndex, picklee.asInstanceOf[Int]),
          (picklee: Any) => boundStatement.setInt(popName, picklee.asInstanceOf[Int])
        ),
        FastTypeTag.Long.key -> (
          (picklee: Any) => boundStatement.setLong(nextIndex, picklee.asInstanceOf[Long]),
          (picklee: Any) => boundStatement.setLong(popName, picklee.asInstanceOf[Long])
        ),
        FastTypeTag.Short.key -> (
          (picklee: Any) => boundStatement.setInt(nextIndex, picklee.asInstanceOf[Short]),
          (picklee: Any) => boundStatement.setInt(popName, picklee.asInstanceOf[Short])
        ),
        FastTypeTag.Double.key -> (
          (picklee: Any) => boundStatement.setDouble(nextIndex, picklee.asInstanceOf[Double]),
          (picklee: Any) => boundStatement.setDouble(popName, picklee.asInstanceOf[Double])
        ),
        FastTypeTag.Float.key -> (
          (picklee: Any) => boundStatement.setFloat(nextIndex, picklee.asInstanceOf[Float]),
          (picklee: Any) => boundStatement.setFloat(popName, picklee.asInstanceOf[Float])
        ),
        FastTypeTag.Boolean.key -> (
          (picklee: Any) => boundStatement.setBool(nextIndex, picklee.asInstanceOf[Boolean]),
          (picklee: Any) => boundStatement.setBool(popName, picklee.asInstanceOf[Boolean])
        ),
        FastTypeTag.Byte.key -> (
          (picklee: Any) => boundStatement.setInt(nextIndex, picklee.asInstanceOf[Byte]),
          (picklee: Any) => boundStatement.setInt(popName, picklee.asInstanceOf[Byte])
        ),
        FastTypeTag.Char.key -> (
          (picklee: Any) => boundStatement.setString(nextIndex, picklee.asInstanceOf[Char].toString),
          (picklee: Any) => boundStatement.setString(popName, picklee.asInstanceOf[Char].toString)
        ),
        FastTypeTag.ScalaString.key -> (
          (picklee: Any) => boundStatement.setString(nextIndex, picklee.toString),
          (picklee: Any) => boundStatement.setString(popName, picklee.toString)
        ),
        FastTypeTag.JavaString.key -> (
          (picklee: Any) => boundStatement.setString(nextIndex, picklee.toString),
          (picklee: Any) => boundStatement.setString(popName, picklee.toString)
        ),
        FastTypeTag.ArrayByte.key -> (
          (picklee: Any) => boundStatement.setBytes(nextIndex, ByteBuffer.wrap(picklee.asInstanceOf[Array[Byte]])),
          (picklee: Any) => boundStatement.setBytes(popName, ByteBuffer.wrap(picklee.asInstanceOf[Array[Byte]]))
        ),
        FastTypeTag.ArrayShort.key -> (
          (picklee: Any) => boundStatement.setList[Short](nextIndex, picklee.asInstanceOf[Array[Short]].toList),
          (picklee: Any) => boundStatement.setList[Short](popName, picklee.asInstanceOf[Array[Short]].toList)
        ),
        FastTypeTag.ArrayChar.key -> (
          (picklee: Any) => boundStatement.setList[Char](nextIndex, picklee.asInstanceOf[Array[Char]].toList),
          (picklee: Any) => boundStatement.setList[Char](popName, picklee.asInstanceOf[Array[Char]].toList)
        ),
        FastTypeTag.ArrayInt.key -> (
          (picklee: Any) => boundStatement.setList[Int](nextIndex, picklee.asInstanceOf[Array[Int]].toList),
          (picklee: Any) => boundStatement.setList[Int](popName, picklee.asInstanceOf[Array[Int]].toList)
        ),
        FastTypeTag.ArrayLong.key -> (
          (picklee: Any) => boundStatement.setList[Long](nextIndex, picklee.asInstanceOf[Array[Long]].toList),
          (picklee: Any) => boundStatement.setList[Long](popName, picklee.asInstanceOf[Array[Long]].toList)
        ),
        FastTypeTag.ArrayBoolean.key -> (
          (picklee: Any) => boundStatement.setList[Boolean](nextIndex, picklee.asInstanceOf[Array[Boolean]].toList),
          (picklee: Any) => boundStatement.setList[Boolean](popName, picklee.asInstanceOf[Array[Boolean]].toList)
        ),
        FastTypeTag.ArrayFloat.key -> (
          (picklee: Any) => boundStatement.setList[Float](nextIndex, picklee.asInstanceOf[Array[Float]].toList),
          (picklee: Any) => boundStatement.setList[Float](popName, picklee.asInstanceOf[Array[Float]].toList)
        ),
        FastTypeTag.ArrayDouble.key -> (
          (picklee: Any) => boundStatement.setList[Double](nextIndex, picklee.asInstanceOf[Array[Double]].toList),
          (picklee: Any) => boundStatement.setList[Double](popName, picklee.asInstanceOf[Array[Double]].toList)
        )
      )

  def beginEntry(picklee: Any): PBuilder = beginEntry(picklee, (hints: Hints) => {
    if (primitives.contains(hints.tag.key)) {

      if (useIndex) {
        primitives(hints.tag.key)._1(picklee)
      }
      else {
        primitives(hints.tag.key)._2(picklee)
      }
    }
    else
      if(hints.tag.key.startsWith("scala.Some[")) {
        levelSome += 1;
    }
     /*else {
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
    if (levelSome <= 0) {
      names.push(name) // if levelSome > 1 then is always Option field 'x', this has no meaning
    }
    pickler(this)
    this
  }

  def endEntry(): Unit = {
    if (primitives.contains(tags.pop().key)) () // do nothing
    if (levelSome > 0)
      levelSome -= 1;
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
        boundStatement.setDate(nextIndex, date)
      }
      else {
        boundStatement.setDate(popName, date)
      }
    })
    endEntry()
  }

  def pickleSet[T](set: Set[T]) {
    beginEntry(set, (hints: Hints) => {
      if (useIndex) {
        boundStatement.setSet[T](nextIndex, set)
      }
      else {
        boundStatement.setSet[T](popName, set)
      }
    })
    endEntry()
  }

  def pickleNullable[T](picklee: Option[T]) {
    if (picklee.isDefined) {
      beginEntry(picklee.get)
    }
    if (useIndex)
      nextIndex
    else
      popName
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

  def atPrimitive: Boolean = primitives.contains(lastReadTag.key) || lastReadTag.key.startsWith("scala.Option[")

  def readPrimitive(): Any = {

    // sad dirty hack to read Option[T] :(
    if (lastReadTag.key.startsWith("scala.Option[")) {
      if (row.isNull(fieldName))
        None
      else {
        val s1 = lastReadTag.key.substring("scala.Option[".length)
        val key = s1.substring(0, s1.length-1)
        Some(primitives(key)())
      }
    }
    else {
      primitives(lastReadTag.key)()
    }
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
  var nextIndex: Integer = 0
  var bs: BoundStatement = bsoInitial
  def put(obj: com.datastax.driver.core.BoundStatement): this.type = { bs = obj; ??? }
  def result = bs
}