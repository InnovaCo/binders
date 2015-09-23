package eu.inn.binders.dynamic.internal

import eu.inn.binders.dynamic.Value

import scala.language.experimental.macros
import scala.language.reflectiveCalls
import scala.reflect.macros.Context

private [dynamic] trait DynamicMacroImpl {
  val c: Context
  import c.universe._

  def fromDynamic[O: c.WeakTypeTag]: c.Tree = {
    val t = fresh("t")
    val block = q"""{
      val $t = ${c.prefix.tree}
      ValueSerializerFactory.findFactory().withDeserializer[${weakTypeOf[O]}]($t.value, dsrz$$666 => {
        dsrz$$666.unbind[${weakTypeOf[O]}]
      })
    }"""
    //println(block)
    block
  }

  def toDynamic[O: c.WeakTypeTag]: c.Tree = {
    val t = fresh("t")
    val block = q"""{
      val $t = ${c.prefix.tree}
      ValueSerializerFactory.findFactory().withSerializer((srlz$$666)=> {
        srlz$$666.bind[${weakTypeOf[O]}]($t.obj)
      })
    }"""
    //println(block)
    block
  }

  def selectDynamic[O: c.WeakTypeTag](name: c.Expr[String]): c.Tree = {
    val t = fresh("t")
    val Literal(Constant(defName: String)) = name.tree
    val fieldName = readerNameToField(defName)
    val tpe = weakTypeOf[O]

    val block =
    if (tpe <:< typeOf[Option[_]])
      q"""{
      import eu.inn.binders.dynamic._
      val $t = ${c.prefix.tree}
      $t.asMap.get($fieldName).map(_.fromDynamic[$tpe]).flatten
      }"""
    else
      q"""{
      import eu.inn.binders.dynamic._
      val $t = ${c.prefix.tree}
      $t.asMap.get($fieldName).map(_.fromDynamic[$tpe]).getOrElse(throw new eu.inn.binders.core.FieldNotFoundException($fieldName))
      }"""
    //println(block)
    block
  }

  def readerNameToField(readerName: String) = if (readerName.startsWith("_") && readerName.length > 1) {
    readerName.substring(1)
  } else {
    readerName
  }

  def fresh(prefix: String): TermName = newTermName(c.fresh(prefix))
}
