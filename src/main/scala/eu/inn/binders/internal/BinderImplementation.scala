package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

private trait BinderImplementation {
  val c: Context
  import c.universe._

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag] (stmt: c.Tree, index: c.Tree, obj: c.Tree, allFields: Boolean): c.Tree = {

    val setters = extractSetters[S]
    // println("setters: " + setters)

    val stmtTerm = newTermName(c.fresh("$stmt"))
    val indexTerm = newTermName(c.fresh("$index"))
    val objTerm = newTermName(c.fresh("$obj"))
    val vals = List(
      ValDef(Modifiers(), stmtTerm, TypeTree(), stmt),
      ValDef(Modifiers(), indexTerm, TypeTree(), index),
      ValDef(Modifiers(), objTerm, TypeTree(), obj)
    )

    // println(wholeParamType)
    val wholeParamSetter = findSetter(true, setters, obj.symbol)

    // println (wholeParamSetter)
    val listOfCalls : List[Tree] = wholeParamSetter match {
      case Some(m) => {
        List(Apply(Select(Ident(stmtTerm),TermName(m.name.decoded)),
          List(Ident(indexTerm.decoded), Ident(objTerm.decoded))))
      }

      case None => {
        val caseClassParams = extractCaseClassParams[O]

        caseClassParams.flatMap { parameter =>
            // println("looking setter for " + parameter + " in " + setters)
            findSetter(false, setters, parameter).map { setter =>

              // println("found setter for " + parameter + " : " + setter)
              val setterCall = Apply(Select(Ident(stmtTerm),TermName(setter.name.decoded)),
                List(Literal(Constant(parameter.name.decoded)),
                  Select(Ident(objTerm.decoded),TermName(parameter.name.decoded)))
              )

              val hasCall = Apply(Select(Ident(stmtTerm), "hasParameter"), List(Literal(Constant(parameter.name.decoded))))

              if (allFields)
                setterCall
              else
                If(hasCall, setterCall, Literal(Constant()))
            }
          }
      }
    }

    val block = Block(vals ++ listOfCalls, Literal(Constant()))
    //println(block)
    block
  }

  def createFrom[R: c.WeakTypeTag, O: c.WeakTypeTag] (row: c.Tree): c.Tree = {

    val getters = extractGetters[R]
    // println("getters: " + getters)

    val caseClassParams = extractCaseClassParams[O]
    // println(caseClassParams)

    val rowTerm = newTermName(c.fresh("$row"))
    val objTerm = newTermName(c.fresh("$obj"))

    val applyParams =
      caseClassParams.map { parameter =>
        val getter = findGetter(getters, parameter)

        Apply(Select(Ident(rowTerm),TermName(getter.name.decoded)),
          List(Literal(Constant(parameter.name.decoded))))
      } toList

    val outputCompanionSymbol = weakTypeOf[O].typeSymbol.companionSymbol
    val applyCall = Apply(Select(Ident(outputCompanionSymbol.name), "apply"),applyParams)

    val vals = List(
      ValDef(Modifiers(), rowTerm, TypeTree(), row),
      ValDef(Modifiers(), objTerm, TypeTree(), applyCall)
    )
    val block = Block(vals, Ident(objTerm))
    // println(block)
    block
  }

  def fillFrom[R: c.WeakTypeTag, O: c.WeakTypeTag] (row: c.Tree, obj: c.Tree): c.Tree = {

    val getters = extractGetters[R]
    // println("getters: " + getters)

    val caseClassParams = extractCaseClassParams[O]
    // println(caseClassParams)

    val rowTerm = newTermName(c.fresh("$row"))
    val objResultTerm = newTermName(c.fresh("$obj"))
    val objOrigTerm = newTermName(c.fresh("$objOrig"))

    val applyParams =
      caseClassParams.map { parameter =>
        val getter = findGetter(getters, parameter)

      
        val fromRow = Apply(Select(Ident(rowTerm),TermName(getter.name.decoded)),
          List(Literal(Constant(parameter.name.decoded))))
        
        val fromObjOrig =
            Select(Ident(objOrigTerm.decoded),TermName(parameter.name.decoded))

        val hasCall = Apply(Select(Ident(rowTerm), "hasField"), List(Literal(Constant(parameter.name.decoded))))

        If(hasCall, fromRow, fromObjOrig)
      } toList

    val outputCompanionSymbol = weakTypeOf[O].typeSymbol.companionSymbol
    val applyCall = Apply(Select(Ident(outputCompanionSymbol.name), "apply"),applyParams)

    val vals = List(
      ValDef(Modifiers(), rowTerm, TypeTree(), row),
      ValDef(Modifiers(), objOrigTerm, TypeTree(), obj),
      ValDef(Modifiers(), objResultTerm, TypeTree(), applyCall)
    )
    val block = Block(vals, Ident(objResultTerm))
    // println(block)
    block
  }

  def findSetter(byIndex: Boolean, setters: List[MethodSymbol], parameter: c.Symbol) : Option[MethodSymbol] = {

    var exactMatch: Option[MethodSymbol] = None
    var baseMatch: Option[MethodSymbol] = None
    for (m <- setters) {

      val idxSymbol = m.paramss.head(0); // parameter 1 (index/name)
      val valueSymbol = m.paramss.head(1); // parameter 2 (value)

      //println("Comparing " + m + " with type " + valueSymbol.typeSignature + " for parameter " + parameter + " with type " + parameter.typeSignature)

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
        ( m.paramss.head(0).typeSignature =:= typeOf[Int] ||
          m.paramss.head(0).typeSignature =:= typeOf[String] )
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  def findGetter(getters: List[MethodSymbol], parameter: c.Symbol) : MethodSymbol = {

    var exactMatch: Option[MethodSymbol] = None
    var baseMatch: Option[MethodSymbol] = None

    for (m <- getters) {
      if (parameter.typeSignature =:= m.returnType) {
        exactMatch = Some(m)
      }
      else
      if (m.returnType <:< parameter.typeSignature) {
        baseMatch = Some(m)
      }
    }

    if (exactMatch.isDefined)
      exactMatch.get
    else
    if (baseMatch.isDefined)
      baseMatch.get
    else
      c.abort(c.enclosingPosition, "No getter function found for parameter " + parameter)
  }
  
  def extractGetters[T: c.WeakTypeTag] : List[MethodSymbol] = {

    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("get") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramss.tail.isEmpty && // 1 group of parameters
        m.paramss.head.size == 1 && // only 1 parameter
        m.paramss.head(0).typeSignature =:= typeOf[String]
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  def extractCaseClassParams[T: c.WeakTypeTag] : List[c.Symbol] = {

    val companioned = weakTypeOf[T].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    companionType.declaration(stringToTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, s"No unapply function found for ${companioned.fullName}")
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