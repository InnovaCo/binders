package eu.inn


import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

object ExperimentalImpl {


  def serialize[F: c.WeakTypeTag, T: c.WeakTypeTag]
    (c: Context)
    (from: c.Expr[F], to: c.Expr[T]): c.Expr[Unit] = {
    import c.universe._

    val setters = extractSetters[T](c)
    val caseClassParams = extractCaseClassParams[F](c)

    caseClassParams.map (param =>
      reify()

      //Apply(Select(Ident(newTermName("to")), newTermName("setInt")),
      //  List(Literal(Constant(1))))))
    )






    val tree = Literal(Constant(2))

    println(from)
    println(to)
    val f = Literal(Constant("limit exceeded"))
    c.Expr[Unit](tree)
  }

  def extractSetters[T: c.WeakTypeTag](c: Context) : List[c.Symbol] = {
    import c.universe._

    weakTypeOf[T].members.filter(member =>
        member.isMethod && member.name.decoded.startsWith("set") && member.isPublic
    ).toList
  }

  def extractCaseClassParams[T: c.WeakTypeTag](c: Context) : List[c.Symbol] = {
    import c.universe._

    val companioned = weakTypeOf[T].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    companionType.declaration(stringToTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, "No unapply function found")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Apply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t @ TypeRef(_, _, Nil) => Some(List(t))
              case t @ TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        println("Unapply return type:" + unapply.returnType)
        println("Unapply return types:" + unapplyReturnTypes)

        companionType.declaration(stringToTermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asMethod.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>

                println("apply found:" + apply)
                if (!apply.paramss.tail.isEmpty)
                  c.abort(c.enclosingPosition, "Couldn't use apply method with more than a single parameter group")

                apply.paramss.head

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }
        }

    }
  }

}