package eu.inn

import language.experimental.macros
import eu.inn.internal.BinderImpl

object Binder {
  def bindInto[F,T](from: F, to: T, index: Int) : Unit = macro BinderImpl.bindInto[F,T]
}
