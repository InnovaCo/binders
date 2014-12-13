package eu.inn.binders.internal

import scala.reflect.ClassTag

object UnbindImplicits {
  implicit def IteratorToList[T](i:Iterator[T]):List[T]=i.toList
//  implicit def IteratorToSeq[T](i:Iterator[T]):Seq[T]=i.toSeq
  implicit def IteratorToArray[T : ClassTag](i:Iterator[T]):Array[T]=i.toArray
  implicit def IteratorToVector[T](i:Iterator[T]):Vector[T]=i.toVector
  implicit def IteratorToIndexedSeq[T](i:Iterator[T]):IndexedSeq[T]=i.toIndexedSeq
}
