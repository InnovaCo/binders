package eu.inn.binders.internal

import eu.inn.binders.core.FieldNotFoundException

object Helpers {
   def getFieldOrThrow[T](x: Option[T], fieldName: String):T = {
     x.getOrElse(throw new FieldNotFoundException(fieldName))
   }
 }
