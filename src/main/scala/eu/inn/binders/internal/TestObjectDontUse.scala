package eu.inn.binders.internal

/*

todo: move test class to separate test jar (doesn't work with unit-tests as it's same compilation unit)

import eu.inn.binders.core.Statement

object TestObjectDontUse {
   import scala.language.experimental.macros
   import scala.reflect.macros.Context

   implicit class TestStringContext(sc: StringContext) {
     def test[S <: Statement[_]](args: Any*)(implicit statement: S) = macro testBindImpl[S]
   }

   def testBindImpl[S: c.WeakTypeTag]
   (c: Context)
   (args: c.Expr[Any]*)
   (statement: c.Expr[S]): c.Expr[Unit] = {
     import c.universe._

     val bindCall = Apply(
       Select(statement.tree, newTermName("bindArgs")),
       args.map(_.tree).toList
     )

     c.Expr[Unit](Block(
      List(bindCall),
      Literal(Constant())
     ))
   }
 }

*/