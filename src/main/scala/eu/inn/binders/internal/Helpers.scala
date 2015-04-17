package eu.inn.binders.internal

import eu.inn.binders.core.FieldNotFoundException
import eu.inn.binders.dynamic._

object Helpers {
  def getFieldOrThrow[T](x: Option[T], fieldName: String): T = {
    if (x == null)
      throw new FieldNotFoundException(fieldName)
    x.getOrElse(throw new FieldNotFoundException(fieldName))
  }

  def getConformity(typ:String, value: DynamicValue): Int= {
    value.accept(new DynamicVisitor[Int] {
      override def visitNumber(d: Number): Int = if (typ == "Number") return 100 else 0
      override def visitBool(d: Bool): Int = if (typ == "Bool") return 100 else 0
      override def visitObj(d: Obj): Int = if (typ == "Obj") return 100 else 0
      override def visitText(d: Text): Int = if (typ == "Text") return 100 else 0
      override def visitLst(d: Lst): Int = if (typ == "Lst") return 100 else 0
    })
  }
}