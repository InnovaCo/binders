package eu.inn

import language.experimental.macros

/*trait OutputColumn {
  val name: String
}*/

trait Output {
  //val columns: Array[OutputColumn]

  def hasColumn(name: String) : Boolean = true
}

object Experimental {


  def serialize[F,T <: Output](from: F, to: T) : Unit = macro ExperimentalImpl.serialize[F,T]


}
