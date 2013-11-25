package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros

private trait BinderImplementation {
  val c: Context
  import c.universe._

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag] (index: c.Tree, obj: c.Tree, allFields: Boolean): c.Tree = {

    val setters = extractSetters[S]
    // println("setters: " + setters)

    val thisTerm = TermName(c.fresh("$this"))
    val stmtTerm = newTermName(c.fresh("$stmt"))
    val indexTerm = newTermName(c.fresh("$index"))
    val objTerm = newTermName(c.fresh("$obj"))
    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), stmtTerm, TypeTree(), Select(Ident(thisTerm), TermName("stmt"))),
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

  def unbind[R: c.WeakTypeTag, O: c.WeakTypeTag] : c.Tree = {

    val getters = extractGetters[R]
    // println("getters: " + getters)

    val caseClassParams = extractCaseClassParams[O]
    // println(caseClassParams)

    val thisTerm = TermName(c.fresh("$this"))
    val rowTerm = TermName(c.fresh("$row"))
    val objTerm = TermName(c.fresh("$obj"))

    val applyParams =
      caseClassParams.map { parameter =>
        val getter = findGetter(getters, parameter)

        Apply(Select(Ident(rowTerm),TermName(getter.name.decoded)),
          List(Literal(Constant(parameter.name.decoded))))
      } toList

    val outputCompanionSymbol = weakTypeOf[O].typeSymbol.companionSymbol
    val applyCall = Apply(Select(Ident(outputCompanionSymbol.name), "apply"),applyParams)

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), rowTerm, TypeTree(), Select(Ident(thisTerm), TermName("row"))),
      ValDef(Modifiers(), objTerm, TypeTree(), applyCall)
    )
    val block = Block(vals, Ident(objTerm))
    // println(block)
    block
  }

  def unbindPartial[R: c.WeakTypeTag, O: c.WeakTypeTag] (obj: c.Tree): c.Tree = {

    val getters = extractGetters[R]
    // println("getters: " + getters)

    val caseClassParams = extractCaseClassParams[O]
    // println(caseClassParams)

    val thisTerm = TermName(c.fresh("$this"))
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
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), rowTerm, TypeTree(), Select(Ident(thisTerm), TermName("row"))),
      ValDef(Modifiers(), objOrigTerm, TypeTree(), obj),
      ValDef(Modifiers(), objResultTerm, TypeTree(), applyCall)
    )
    val block = Block(vals, Ident(objResultTerm))
    // println(block)
    block
  }

  def unbindOne[RS: c.WeakTypeTag, O: c.WeakTypeTag] : c.Tree = {
    val thisTerm = TermName(c.fresh("$this"))
    val rowsTerm = TermName(c.fresh("$rows"))
    val iteratorTerm = TermName(c.fresh("$iterator"))
    val objResultTerm = TermName(c.fresh("$result"))

    val hasCall = Select(Ident(iteratorTerm), "hasNext")

    // get first element
    val nextRowTerm = TermName(c.fresh("$nextRow"))
    val nextRowCall = Apply(Select(Ident(iteratorTerm), "next"), List())
    val valsNextItem = List(
      ValDef(Modifiers(), nextRowTerm, Select(Ident(rowsTerm), TypeName("rowType")), nextRowCall)
    )

    val unbindCall = Block(valsNextItem,
      Apply(Select(Ident(weakTypeOf[Some.type].termSymbol), "apply"),
        List(
          TypeApply(Select(Ident(nextRowTerm), "unbind"), List(Ident(weakTypeOf[O].typeSymbol)))
        )
      )
    )
    
    val none = weakTypeOf[None.type].termSymbol
    val ifCall = If(hasCall, unbindCall, Ident(none))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), rowsTerm, TypeTree(), Select(Ident(thisTerm), TermName("rows"))),
      ValDef(Modifiers(), iteratorTerm, TypeTree(), Select(Ident(rowsTerm), TermName("iterator"))),
      ValDef(Modifiers(), objResultTerm, TypeTree(), ifCall)
    )

    val block = Block(vals, Ident(objResultTerm))
    // println(block)
    block
  }

  def unbindAll[RS: c.WeakTypeTag, O: c.WeakTypeTag] : c.Tree = {
    val thisTerm = TermName(c.fresh("$this"))
    val rowsTerm = TermName(c.fresh("$rows"))
    val rowTerm = TermName(c.fresh("$row"))
    val iteratorTerm = TermName(c.fresh("$iterator"))
    val objResultTerm = TermName(c.fresh("$result"))

    val unbindCall = Function(
      List(ValDef(Modifiers(Flag.PARAM), rowTerm, Select(Ident(rowsTerm), TypeName("rowType")), EmptyTree)),
      TypeApply(Select(Ident(rowTerm), "unbind"), List(Ident(weakTypeOf[O].typeSymbol)))
    )

    // map iterator
    val mapCall = Apply(Select(Ident(iteratorTerm), "map"), List(unbindCall))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), rowsTerm, TypeTree(), Select(Ident(thisTerm), TermName("rows"))),
      ValDef(Modifiers(), iteratorTerm, TypeTree(), Select(Ident(rowsTerm), TermName("iterator"))),
      ValDef(Modifiers(), objResultTerm, TypeTree(), mapCall)
    )

    val block = Block(vals, Ident(objResultTerm))
    //println(block)
    block
  }

  def execute (args: Seq[c.Tree]): c.Tree = {

    val thisTerm = TermName(c.fresh("$this"))
    val rowsTerm = TermName(c.fresh("$rows"))
    val stmtTerm = TermName(c.fresh("$stmt"))

    var bindAllParameters =
      args.zipWithIndex.map { arg =>
        val t = arg._1
        val index = arg._2
        val term = TermName(c.fresh("$t0"))
        val vdef = ValDef(Modifiers(), term, TypeTree(), t)
        val bindCall = Apply(Select(Ident(stmtTerm), "bind"), List(Literal(Constant(index)),Ident(term)))
        List(vdef,bindCall)
      } flatten

    val bindCallback = Function(
      List(ValDef(Modifiers(Flag.PARAM), stmtTerm, Select(Ident(thisTerm), TypeName("statementType")), EmptyTree)),
      Block(bindAllParameters.toList, Literal(Constant()))
    )

    val callExecute = Apply(
      Select(Select(Ident(thisTerm), "query"), "bindAndExecute"), List(bindCallback)
    )

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), rowsTerm, TypeTree(), callExecute)
    )

    val block = Block(vals, Ident(rowsTerm))
    println(block)
    block
  }



  private def findSetter(byIndex: Boolean, setters: List[MethodSymbol], parameter: c.Symbol) : Option[MethodSymbol] = {

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
  
  private def extractSetters[T: c.WeakTypeTag] : List[MethodSymbol] = {

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

  private def findGetter(getters: List[MethodSymbol], parameter: c.Symbol) : MethodSymbol = {

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

  private def extractGetters[T: c.WeakTypeTag] : List[MethodSymbol] = {

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

  private def extractCaseClassParams[T: c.WeakTypeTag] : List[c.Symbol] = {

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

  private object TermName {
    def apply(s: String) = newTermName(s)
    def unapply(name: TermName): Option[String] = Some(name.toString)
  }

  private object TypeName {
    def apply(s: String) = newTypeName(s)
    def unapply(name: TypeName): Option[String] = Some(name.toString)
  }
}
