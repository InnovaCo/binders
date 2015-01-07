package eu.inn.binders.internal

import eu.inn.binders.core.{Serializer, FieldNotFoundException}

object Helpers {
  /*def f[T,C](s : Serializer[C], block: => T) : T = {
    try {
      block
    }
    finally {
      s.close()
    }
  }*/

   def getFieldOrThrow[T](x: Option[T], fieldName: String):T = {
     x.getOrElse(throw new FieldNotFoundException(fieldName))
   }
 }
