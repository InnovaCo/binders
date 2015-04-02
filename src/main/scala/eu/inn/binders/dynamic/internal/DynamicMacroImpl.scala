package eu.inn.binders.dynamic.internal

import eu.inn.binders.dynamic.DynamicObject

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

trait DynamicMacroImpl {
  val c: Context
  import c.universe._

  def fromDynamic[O: c.WeakTypeTag]: c.Tree = {
    val block = q"""{
      val t = ${c.prefix.tree}
      DynamicSerializerFactory.findFactory().withDeserializer[${weakTypeOf[O]}](t.dynamic, deserializer=> {
        deserializer.unbind[${weakTypeOf[O]}]
      })
    }"""
    //println(block)
    block
  }

  def toDynamic[O: c.WeakTypeTag]: c.Tree = {
    val block = q"""{
      val t = ${c.prefix.tree}
      DynamicSerializerFactory.findFactory().withSerializer(serializer=> {
        serializer.bind[${weakTypeOf[O]}](t.obj)
      })
    }"""
    //println(block)
    block
  }
}
