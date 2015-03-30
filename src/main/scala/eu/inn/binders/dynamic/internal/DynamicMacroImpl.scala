package eu.inn.binders.dynamic.internal
import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

trait DynamicMacroImpl {
  val c: Context
  import c.universe._

  def parseJson[O: c.WeakTypeTag]: c.Tree = {
    val block = q"""{
      val t = ${c.prefix.tree}
      SerializerFactory.findFactory().withParser[${weakTypeOf[O]}](t.jsonString, deserializer=> {
        deserializer.unbind[${weakTypeOf[O]}]
      })
    }"""
    //println(block)
    block
  }

  def toJson[O: c.WeakTypeTag]: c.Tree = {
    val block = q"""{
      val t = ${c.prefix.tree}
      SerializerFactory.findFactory().withGenerator(serializer=> {
        serializer.bind[${weakTypeOf[O]}](t.obj)
      })
    }"""
    //println(block)
    block
  }

  def writeMap[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val block = q"""{
      val serializer = ${c.prefix.tree}
      val it = $value
      serializer.beginObject()
      it.foreach(kv => {
        serializer.getFieldSerializer(kv._1).map(_.bind(kv._2))
      })
      serializer.endObject()
    }"""
    //println(block)
    block
  }

  def readMap[S: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val block = q"""{
      val deserializer = ${c.prefix.tree}
      deserializer.iterator().map{ el =>
        (el.fieldName.get, el.unbind[${weakTypeOf[O]}])
      }.toMap
    }"""
    //println(block)
    block
  }
}
