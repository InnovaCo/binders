package eu.inn.binders.core

trait ImplicitDeserializer[T, D <: Deserializer[_]] {
   def read(deserializer: D): T
}
