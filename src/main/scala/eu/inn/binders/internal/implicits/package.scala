package eu.inn.binders.internal

package object implicits {
  implicit def itos[T](i:Iterator[T]):Seq[T]=i.toSeq
}
