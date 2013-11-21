package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

trait DbPicklingSerializer {
  val c: Context
  import c.universe._

  def serialize[F: c.WeakTypeTag, T: c.WeakTypeTag] (from: c.Tree, to: c.Tree): c.Tree = {

    import c.universe._
    import definitions._
    import c.universe.Flag._

    val setters = extractSetters[T]
    // println("setters: " + setters)

    val wholeParamType = weakTypeOf[F].typeSymbol
    val v = ValDef(Modifiers(), newTermName("obj"), TypeTree(), from)

    println(wholeParamType)

    val wholeParamSetter = findSetter(true, setters, wholeParamType)

    println (wholeParamSetter)
    val listOfCalls : List[Tree] = wholeParamSetter match {
      case m: MethodSymbol => {
        List(Apply(Select(to,TermName(m.name.decoded)),
          List(Ident("obj"))))
      }

      case _ => {
        val caseClassParams = extractCaseClassParams[F]

        List(v) ++ caseClassParams.flatMap { parameter =>
            // println("looking setter for " + parameter + " in " + setters)
            findSetter(false, setters, parameter).map { setter =>

              // println("found setter for " + parameter + " : " + setter)
              Apply(Select(to,TermName(setter.name.decoded)),
                List(Literal(Constant(parameter.name.decoded)),
                  Select(Ident("obj"),TermName(parameter.name.decoded)))
              )
            }
          }
      }
    }

    val block = Block(listOfCalls, Literal(Constant()))
    println(block)
    block
  }

  def findSetter(byIndex: Boolean, setters: List[MethodSymbol], parameter: c.Symbol) : Option[MethodSymbol] = {

    var exactMatch: Option[MethodSymbol] = None
    var baseMatch: Option[MethodSymbol] = None

    for (m <- setters) {
      val idxSymbol = m.paramss.head.head; // parameter 1 (index/name)
      val valueSymbol = m.paramss.head(1); // parameter 2 (value)
      if (if (byIndex) idxSymbol.typeSignature =:= typeOf[Int] else idxSymbol.typeSignature =:= typeOf[String]) {
        if (parameter.typeSignature =:= valueSymbol.typeSignature) {
          exactMatch = Some(m)
        }
        else
        if (parameter.typeSignature <:< valueSymbol.typeSignature) {
          baseMatch = Some(m)
        }
      }
    }

    if (exactMatch.isDefined)
      exactMatch
    else
      baseMatch
  }

  def extractSetters[T: c.WeakTypeTag] : List[MethodSymbol] = {

    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("set") &&
      member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      m.paramss.tail.isEmpty && // 1 group of parameters
        m.paramss.head.size == 2 && // only 2 parameters
        ( m.paramss.head.head.typeSignature =:= typeOf[Int] ||
          m.paramss.head.head.typeSignature =:= typeOf[String] )
    }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  def extractCaseClassParams[T: c.WeakTypeTag] : List[c.Symbol] = {

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

  object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}
