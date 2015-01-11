package eu.inn

import eu.inn.binders.core.{Serializer, Deserializer}

package object binders {

  import eu.inn.internal.BinderProxy
  import language.experimental.macros

  implicit class SerializerOps[S <: Serializer[_]](val serializer: S) {
    def bind[O](value: O): S = macro BinderProxy.bind[S, O]

    def bindArgs(t: Any*): S = macro BinderProxy.bindArgs[S]

    def bindPartial[O <: Product](value: O): S = macro BinderProxy.bindPartial[S, O]
  }

  implicit class DeserializerOps[D <: Deserializer[_]](val deserializer: D) {
    def unbind[O]: O = macro BinderProxy.unbind[D, O]

    def unbindPartial[O](originalValue: O): O = macro BinderProxy.unbindPartial[D, O]
  }
}
