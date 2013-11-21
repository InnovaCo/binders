package eu.inn


import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros
import scala.pickling.Macro

object ExperimentalImpl {

  import scala.reflect.runtime.{universe => ru}

  def serialize[F: c.WeakTypeTag, T: c.WeakTypeTag]
    (c: Context)
    (from: c.Expr[F], to: c.Expr[T]): c.Expr[Unit] = {

    import c.universe._
    import definitions._
    import c.universe.Flag._

    val setters = extractSetters[T](c)
    //println("setters: " + setters)

    val caseClassParams = extractCaseClassParams[F](c)

    //val x = Apply(Select(to.tree, newTermName("setInt")), List(Literal(Constant(1))))

    val x = q"""
    """

    /*val listOfCalls = caseClassParams.map { parameter =>
        findSetter(c)(false, setters, parameter).map { setter =>
        //println("setter for " + parameter + " : " + setter)
          val callSetter = Apply(
            Select(
              to.tree,
              newTermName(setter.name.decoded)
            ),
            List(Select(
              from.tree,
              newTermName(parameter.name.decoded)
            ))
          )
          callSetter
        }
      }*/

    //val l = for (l <- listOfCalls; if l.isDefined) yield l.get

    //println(l)
    //val block = Block(listOfCalls.map(l => c.Expr(l)))


    //val tree = l.head.get.tre

    val f = Literal(Constant("limit exceeded"))
    c.Expr[Unit](f)
  }

  def findSetter(c: Context)(byIndex: Boolean, setters: List[c.Symbol], parameter: c.Symbol) : Option[c.Symbol] = {
    import c.universe._

    val exactMatch = setters.find(s => {
      val m = s.asInstanceOf[MethodSymbol]
      val idxSymbol = m.paramss.head.head // parameter 1 (index/name)
      val valueSymbol = m.paramss.head(1) // parameter 2 (value)

      parameter.typeSignature =:= valueSymbol.typeSignature &&
        (if (byIndex) idxSymbol.typeSignature =:= typeOf[Int] else idxSymbol.typeSignature =:= typeOf[String])
      })

    if (!exactMatch.isDefined) {
      setters.find(s => {
        val m = s.asInstanceOf[MethodSymbol]
        val idxSymbol = m.paramss.head.head // parameter 1 (index/name)
        val valueSymbol = m.paramss.head(1) // parameter 2 (value)

        parameter.typeSignature <:< valueSymbol.typeSignature &&
          (if (byIndex) idxSymbol.typeSignature =:= typeOf[Int] else idxSymbol.typeSignature =:= typeOf[String])
      })
    }
    else
      exactMatch
  }

  def extractSetters[T: c.WeakTypeTag](c: Context) : List[c.Symbol] = {
    import c.universe._

    weakTypeOf[T].members.filter(member => member.isMethod &&
          member.name.decoded.startsWith("set") &&
          member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      m.paramss.tail.isEmpty && // 1 group of parameters
      m.paramss.head.size == 2 && // only 2 parameters
        ( m.paramss.head.head.typeSignature =:= typeOf[Int] ||
          m.paramss.head.head.typeSignature =:= typeOf[String] )
    }
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

        //println("Unapply return type:" + unapply.returnType)
        //println("Unapply return types:" + unapplyReturnTypes)

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

                //println("apply found:" + apply)
                if (!apply.paramss.tail.isEmpty)
                  c.abort(c.enclosingPosition, "Couldn't use apply method with more than a single parameter group")

                apply.paramss.head

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }
        }

    }
  }

}