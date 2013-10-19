package com.maqdev

import scala.language.implicitConversions

package object capickling {
  implicit def pickleFormat/*(implicit out: BoundStatement): CassandraPickleFormat*/ = new CassandraPickleFormat
  implicit def toCassandraPickle(value: com.datastax.driver.core.Row): CassandraPickle = CassandraPickle(Some(value))
}

import scala.pickling._
import scala.reflect.runtime.universe._
import definitions._
import scala.util.parsing.json._
import scala.collection.mutable.{StringBuilder, Stack}
import com.datastax.driver.core.{BoundStatement, Row}

case class CassandraPickle(value: Option[com.datastax.driver.core.Row]) extends Pickle {
  type ValueType = Option[Row]
  type PickleFormatType = CassandraPickleFormat
}

class CassandraPickleFormat/*(implicit out: BoundStatement)*/ extends PickleFormat {
  type PickleType = CassandraPickle
  type OutputType = BoundStatement

  def createBuilder() = throw new NotImplementedError()// new CassandraPickleBuilder(this, out)

  // TODO: construct
  def createBuilder(out: BoundStatement): PBuilder = new CassandraPickleBuilder(this, out)

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
        FastTypeTag.ArrayByte.key -> ((picklee: Any) => out.setBytes(nextIndex, ByteBuffer.wrap(picklee.asInstanceOf[Array[Byte]]))),
        FastTypeTag.ArrayShort.key -> ((picklee: Any) => out.setList[Short](nextIndex, picklee.asInstanceOf[Array[Short]].toList)),
        FastTypeTag.ArrayChar.key -> ((picklee: Any) => out.setList[Char](nextIndex, picklee.asInstanceOf[Array[Char]].toList)),
        FastTypeTag.ArrayInt.key -> ((picklee: Any) => out.setList[Int](nextIndex, picklee.asInstanceOf[Array[Int]].toList)),
        FastTypeTag.ArrayLong.key -> ((picklee: Any) => out.setList[Long](nextIndex, picklee.asInstanceOf[Array[Long]].toList)),
        FastTypeTag.ArrayBoolean.key -> ((picklee: Any) => out.setList[Boolean](nextIndex, picklee.asInstanceOf[Array[Boolean]].toList)),
        FastTypeTag.ArrayFloat.key -> ((picklee: Any) => out.setList[Float](nextIndex, picklee.asInstanceOf[Array[Float]].toList)),
        FastTypeTag.ArrayDouble.key -> ((picklee: Any) => out.setList[Double](nextIndex, picklee.asInstanceOf[Array[Double]].toList))
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

  def beginEntry(picklee: Any): PBuilder = withHints {
    hints =>
      if (hints.oid != -1) {
        tags.push(FastTypeTag.Ref)
        // append("{ \"$ref\": " + hints.oid + " }") TODO: make something here
      } else {
        tags.push(hints.tag)
        if (primitives.contains(hints.tag.key)) {
          if (hints.isElidedType) primitives(hints.tag.key)(picklee)
          else {
            primitives(hints.tag.key)(picklee)
          }
        } else {
          // if (!hints.isElidedType) append("\"tpe\": \"" + typeToString(hints.tag.tpe) + "\"") TODO: make something here
        }
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

  def beginCollection(length: Int): PBuilder = {
    putField("elems", b => ())
    this
  }

  def putElement(pickler: PBuilder => Unit): PBuilder = {
    pickler(this)
    this
  }

  def endCollection(l: Int): Unit = {
  }

  def result(): CassandraPickle = {
    assert(tags.isEmpty, tags)
    CassandraPickle(None)
  }
}

class CassandraPickleReader(row: Row, val mirror: Mirror, format: CassandraPickleFormat, fieldName: String = "") extends PReader with PickleTools {
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
    FastTypeTag.ArrayShort.key -> (() => (row.getList[Short](fieldName, classOf[Short])).toArray),
    FastTypeTag.ArrayChar.key -> (() => (row.getList[Char](fieldName, classOf[Char])).toArray),
    FastTypeTag.ArrayInt.key -> (() => (row.getList[Int](fieldName, classOf[Int])).toArray),
    FastTypeTag.ArrayLong.key -> (() => (row.getList[Long](fieldName, classOf[Long])).toArray),
    FastTypeTag.ArrayBoolean.key -> (() => (row.getList[Boolean](fieldName, classOf[Boolean])).toArray),
    FastTypeTag.ArrayFloat.key -> (() => (row.getList[Float](fieldName, classOf[Float])).toArray),
    FastTypeTag.ArrayDouble.key -> (() => (row.getList[Double](fieldName, classOf[Double])).toArray)
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

  def beginEntry(): FastTypeTag[_] = withHints {
    hints =>
      lastReadTag = {
        hints.tag
      }
      lastReadTag
  }

  def atPrimitive: Boolean = primitives.contains(lastReadTag.key)

  def readPrimitive(): Any = {
    primitives(lastReadTag.key)()
  }

  def atObject: Boolean = false

  def readField(name: String): CassandraPickleReader = {
    mkNestedReader(name)
  }

  def endEntry(): Unit = {}

  def beginCollection(): PReader = readField("elems")

  def readLength(): Int = throw new NotImplementedError()

  def readElement(): PReader = throw new NotImplementedError()

  def endCollection(): Unit = {}
}