package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros
import eu.inn.binders.naming.Converter

private trait BinderImplementation {
  val c: Context

  import c.universe._

  def bindNext[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val adders = extractAdders[S]
    val tpe = weakTypeOf[O]
    // println("setters: " + setters)

    val thisTerm = newTermName(c.fresh("$this"))
    val serializerTerm = newTermName(c.fresh("$srlz"))
    val objTerm = newTermName(c.fresh("$obj"))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), serializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("serializer"))),
      ValDef(Modifiers(), objTerm, TypeTree(tpe), value)
    )

    val (wholeParamAdder, wholeParamTypeArgs) = findAdder(adders, tpe)

    if (!wholeParamAdder.isDefined) {
      c.abort(c.enclosingPosition, s"No adder function found for parameter with type $tpe")
    }

    // println ("wholeParamSetter = " + wholeParamSetter)
    val listOfCalls: List[Tree] = wholeParamAdder.map { m =>
      makeSetterGetterCall(serializerTerm, m, wholeParamTypeArgs, List(Ident(objTerm)))
    }.toList

    val block = Block(vals ++ listOfCalls, Ident(serializerTerm))
    // println(block)
    block
  }

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree, partial: Boolean): c.Tree = {
    val setters = extractSetters[S]
    // println("setters: " + setters)
    val converter = findConverter[S]

    val thisTerm = newTermName(c.fresh("$this"))
    val serializerTerm = newTermName(c.fresh("$srlz"))
    val objTerm = newTermName(c.fresh("$obj"))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), serializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("serializer"))),
      ValDef(Modifiers(), objTerm, TypeTree(weakTypeOf[O]), value)
    )

    val caseClassParams = extractCaseClassParams[O]

    val listOfCalls: List[Tree] = caseClassParams.flatMap { parameter =>
      // println("looking setter for " + parameter + " in " + setters)

      val (sttr, callTypeArgs) = findSetter(setters, parameter.typeSignature)
      val call = sttr.map { setter =>

        // println("found setter for " + parameter + " : " + setter)
        val setterCall =
          makeSetterGetterCall(serializerTerm, setter, callTypeArgs,
            List(
              parameterLiteral(parameter, converter),
              Select(Ident(objTerm), newTermName(parameter.name.decoded))
            )
          )

        val hasCall = Apply(Select(Ident(serializerTerm), newTermName("hasField")),
          List(parameterLiteral(parameter, converter)))

        if (partial)
          setterCall
        else
          If(hasCall, setterCall, Literal(Constant()))
      }
      if (call.isEmpty) {
        c.abort(c.enclosingPosition, "No setter function found for parameter " + parameter)
      }
      call
    }

    val block = Block(vals ++ listOfCalls, Ident(serializerTerm))
    // println(block)
    block
  }

  def bindArgs(args: Seq[c.Tree]): c.Tree = {
    val thisTerm = newTermName(c.fresh("$this"))
    val serializerTerm = newTermName(c.fresh("$srlz"))

    val bindAllParameters = args.map {
        arg =>
          Apply(
            Select(Ident(serializerTerm), newTermName("bindNext")),
            List(arg)
          )
      }

    val block = Block(
      List(
        ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
        ValDef(Modifiers(), serializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("serializer")))
      ) ++
        bindAllParameters,
      Ident(serializerTerm)
    )
    //println(block)
    block
  }

  def unbind[R: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {

    val companioned = weakTypeOf[O].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    val block = companionType.declaration(newTermName("unapply")) match {
      case NoSymbol => unbindPrimitive[R, O]
      case s => unbindCaseClass[R,O](partial, originalValue)
    }
    // println(s"unbind(${weakTypeTag[R]} -> ${weakTypeTag[O]}):\n $block")
    block
  }

  def unbindPrimitive[R: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[O]
    val casters = extractCasters[R]
    val (casterMethod, casterParamTypeArgs) = findGetter(casters, tpe)

    val thisTerm = newTermName(c.fresh("$this"))
    val deserializerTerm = newTermName(c.fresh("$dsrlz"))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), deserializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("deserializer")))
    )

    val block =
      if (!casterMethod.isDefined) { // found .as converter
        if (tpe <:< typeOf[TraversableOnce[_]]) { // target field is iterable, try to deserialize it as collection
          Block(
            vals,
            convertIterator(
              tpe,
              TypeApply(
                Select(Ident(deserializerTerm), newTermName("unbindAll")),
                List(extractTypeArgs(tpe).head)
              )
            )
          )
        }
        else {
          //println(s"casters: $casters")
          c.abort(c.enclosingPosition, s"No converter function found for ${weakTypeOf[R]} -> $tpe")
        }
      }
      else {
        Block(
          vals,
          applyTypeArgs(Select(Ident(deserializerTerm), casterMethod.get), casterParamTypeArgs, casterMethod.get.typeParams)
          //makeSetterGetterCall(deserializerTerm, casterMethod.get, casterParamTypeArgs, Nothing)
        )
      }

    // println(s"unbindPrimitive(${weakTypeTag[R]} -> ${weakTypeTag[O]}):\n $block")
    block
  }

  def unbindCaseClass[R: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {

    val thisTerm = newTermName(c.fresh("$this"))
    val dsrlzTerm = newTermName(c.fresh("$dsrlz"))
    val objResultTerm = newTermName(c.fresh("$obj"))
    val objOrigTerm = newTermName(c.fresh("$objOrig"))

    val getters = extractGetters[R]
    val converter = findConverter[R]
    val caseClassParams = extractCaseClassParams[O]

    val applyParams: List[(TermName, Tree, Symbol)] =
      caseClassParams.map {
        parameter =>
          val getter = findGetter(getters, parameter.typeSignature)
          val pname = parameterLiteral(parameter, converter)
          val apply = if (getter._1.isDefined) {
            makeSetterGetterCall(dsrlzTerm, getter._1.get, getter._2, List(pname))
          }
          else {
            val innerDslrz = findGetter(getters, weakTypeOf[Option[R]])
            val getc = makeSetterGetterCall(dsrlzTerm, innerDslrz._1.get, innerDslrz._2, List(pname))
            // println("retn = " + parameter.typeSignature)
            if (innerDslrz._1.isDefined) {
              if (parameter.typeSignature <:< typeOf[Option[_]]) {
                val elemTerm = newTermName(c.fresh("$elem"))

                //val typeArgs = extractTypeArgs(parameter.typeSignature) // todo: ?

                val unbindCall = Function(
                  List(ValDef(Modifiers(Flag.PARAM), elemTerm, TypeTree(), EmptyTree)),
                  TypeApply(Select(Ident(elemTerm), newTermName("unbind")), List(TypeTree(parameter.typeSignature)))
                )

                val mapCall = Apply(Select(getc, newTermName("map")), List(unbindCall))
                // println("call = " + mapCall)
                mapCall
              }
              else {
                val getWithCheck = Apply(
                  Select(Ident(typeOf[eu.inn.binders.internal.Helpers.type].termSymbol),
                  newTermName("getFieldOrThrow")), List(getc, pname)
                )

                TypeApply(
                  Select(getWithCheck, newTermName("unbind")),
                  List(TypeTree(parameter.typeSignature))
                )
              }
            }
            else
              c.abort(c.enclosingPosition, s"No getter function found for parameter $parameter: ${parameter.typeSignature}")
          }

        if (partial) {
          val fromObjOrig = Select(Ident(objOrigTerm), newTermName(parameter.name.decoded))
          val hasCall = Apply(Select(Ident(dsrlzTerm), newTermName("hasField")), List(parameterLiteral(parameter, converter)))
          val iff: Tree = If(hasCall, apply, /*else*/ fromObjOrig)
          (newTermName(c.fresh("$arg1")), iff, parameter)
        }
        else {
          (newTermName(c.fresh("$arg1")), apply, parameter)
        }
      }.toList

    val outputCompanionSymbol = weakTypeOf[O].typeSymbol.companionSymbol

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), dsrlzTerm, TypeTree(), Select(Ident(thisTerm), newTermName("deserializer")))
    )

    val applyVals = applyParams.map(p => {
      ValDef(Modifiers(), p._1, TypeTree(p._3.typeSignature), p._2)
    })

    val objOrigVals = if (partial) {
      List(ValDef(Modifiers(), objOrigTerm, TypeTree(), originalValue))
    }
    else
      List()

    val applyCallParams: List[Ident] = applyParams.map(a => Ident(a._1))
    val applyCall = Apply(Select(Ident(outputCompanionSymbol.name), "apply"), applyCallParams)

    val block = Block(vals ++ objOrigVals ++ applyVals ++
      List(ValDef(Modifiers(), objResultTerm, TypeTree(), applyCall)),
      Ident(objResultTerm))

    // println(s"unbindCaseClass(${weakTypeTag[R]} -> ${weakTypeTag[O]}):\n $block")
    block
  }

  def unbindOne[RS: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val thisTerm = newTermName(c.fresh("$this"))
    val deserializerTerm = newTermName(c.fresh("$dsrlz"))
    val iteratorTerm = newTermName(c.fresh("$iterator"))
    val objResultTerm = newTermName(c.fresh("$result"))

    val hasCall = Select(Ident(iteratorTerm), newTermName("hasNext"))

    // get first element
    val nextTerm = newTermName(c.fresh("$next"))
    val nextCall = Apply(Select(Ident(iteratorTerm), newTermName("next")), List())
    val valsNextItem = List(
      ValDef(Modifiers(), nextTerm, TypeTree(), nextCall)
    )

    val unbindCall = Block(valsNextItem,
      Apply(Select(Ident(weakTypeOf[Some.type].termSymbol), "apply"),
        List(
          TypeApply(Select(Ident(nextTerm), newTermName("unbind")), List(TypeTree(weakTypeOf[O])))
        )
      )
    )

    val none = weakTypeOf[None.type].termSymbol
    val ifCall = If(hasCall, unbindCall, Ident(none))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), deserializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("deserializer"))),
      ValDef(Modifiers(), iteratorTerm, TypeTree(), Select(Ident(deserializerTerm), newTermName("iterator"))),
      ValDef(Modifiers(), objResultTerm, TypeTree(), ifCall)
    )

    val block = Block(vals, Ident(objResultTerm))
    // println(block)
    block
  }

  def unbindAll[RS: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val thisTerm = newTermName(c.fresh("$this"))
    val deserializerTerm = newTermName(c.fresh("$dsrlz"))
    val elemTerm = newTermName(c.fresh("$elem"))
    val iteratorTerm = newTermName(c.fresh("$iterator"))
    val objResultTerm = newTermName(c.fresh("$result"))

    val unbindCall = Function(
      List(ValDef(Modifiers(Flag.PARAM), elemTerm, TypeTree(), EmptyTree)),
      TypeApply(Select(Ident(elemTerm), newTermName("unbind")), List(TypeTree(weakTypeOf[O])))
    )

    // map iterator
    val mapCall = Apply(Select(Ident(iteratorTerm), newTermName("map")), List(unbindCall))

    val vals = List(
      ValDef(Modifiers(), thisTerm, TypeTree(), c.prefix.tree),
      ValDef(Modifiers(), deserializerTerm, TypeTree(), Select(Ident(thisTerm), newTermName("deserializer"))),
      ValDef(Modifiers(), iteratorTerm, TypeTree(), Select(Ident(deserializerTerm), newTermName("iterator"))),
      ValDef(Modifiers(), objResultTerm, TypeTree(), mapCall)
    )

    val block = Block(vals, Ident(objResultTerm))
    // println(s"unbindAll(${weakTypeTag[RS]} -> ${weakTypeTag[O]}):\n $block")
    block
  }

  protected def convertIterator(ct: Type, iteratorTree: Tree): Tree = {
    val selector: Option[String] =
      if (ct <:< typeOf[Vector[_]]) {
        Some("toVector")
      }else
      if (ct <:< typeOf[List[_]]) {
        Some("toList")
      }else
      if (ct <:< typeOf[IndexedSeq[_]]) {
        Some("toIndexedSeq")
      }else
      if (ct <:< typeOf[Seq[_]]) {
        Some("toSeq")
      }
      else
        None
    selector.map { s =>
      Select(iteratorTree, newTermName(s))
    } getOrElse {
      iteratorTree
    }
  }

  protected def extractTypeArgs(tpe: Type): List[TypeTree] = {
    tpe match {
      case TypeRef(_, _, args) => args.map(TypeTree(_))
      case _ =>
        c.abort(c.enclosingPosition, s"Can't extract typeArgs from $tpe")
    }
  }

  protected def applyTypeArgs(select: Select, srcTypeArgs: Map[Symbol, Type], dstTypeParams: List[Symbol]) = {
    // println("typeArgs == " + srcTypeArgs + " dstTypes == " + dstTypeParams)
    if (srcTypeArgs.isEmpty || dstTypeParams.isEmpty)
      select
    else {
      TypeApply(select,
        dstTypeParams.map { genericTypeSymbol =>
          srcTypeArgs.get(genericTypeSymbol).map { srcTypeArg =>
            TypeTree(srcTypeArg)
          } getOrElse {
            c.abort(c.enclosingPosition, "Can't find generic arg source for " + select + " / " + genericTypeSymbol)
          }
        } toList)
    }

  }

  protected def makeSetterGetterCall(elemTerm: TermName, method: MethodSymbol, methodTypeArgs: Map[Symbol, Type], parameters: List[Tree]): Apply = {
    val inner = Apply(applyTypeArgs(Select(Ident(elemTerm), method), methodTypeArgs, method.typeParams), parameters)
    if (method.paramss.isEmpty)
      inner
    else
      method.paramss.tail.foldLeft(inner) { (a: Apply, params: List[Symbol]) =>
        Apply(a,
          params.map(p =>
            Select(
              Select(Ident(newTermName("scala")), newTermName("Predef")),
              newTermName("implicitly")
            )
          )
        )
      }
  }

  protected def mostMatching(methods: List[MethodSymbol], scoreFun: MethodSymbol => Option[(Int, Map[Symbol, Type])]): (Option[MethodSymbol], Map[Symbol, Type]) = {
    var rMax: Int = 0
    var mRes: Option[MethodSymbol] = None
    var mTypeArgs: Map[Symbol, Type] = Map()
    methods.map({ m =>
      scoreFun(m) match {
        case Some((r, typeArgs)) =>
          if (r > rMax) {
            rMax = r
            mRes = Some(m)
            mTypeArgs = typeArgs
          }
        case None => // do nothing
      }
      // println("Comparing " + m + " with arg type " + methodParSym.typeSignature + " for parameter " + parSym + " with type " + parSymType + " RES = " + r + " --- " + math.random)
    })
    (mRes, mTypeArgs)
  }

  protected def findAdder(adders: List[MethodSymbol], parSymType: Type): (Option[MethodSymbol], Map[Symbol, Type]) = {
    mostMatching(adders, m => {
      val valuePar = m.paramss.head(0) // parSym 0 - value
      Some(compareTypes(parSymType, valuePar.typeSignature))
    })
  }

  protected def findSetter(setters: List[MethodSymbol], parSymType: Type): (Option[MethodSymbol], Map[Symbol, Type]) = {
    mostMatching(setters, m => {
      val namePar = m.paramss.head(0)    // parSym 0 - name
      val valuePar = m.paramss.head(1)   // parSym 1 - value
      if (namePar.typeSignature =:= typeOf[String])
        Some(compareTypes(parSymType, valuePar.typeSignature))
      else
        None
    })
  }

  protected def compareTypes(left: Type, right: Type): (Int, Map[Symbol, Type]) = {
    if (left =:= right)
      (100, Map())
    else
    if (left <:< right)
      (90, Map())
    else
    if (left weak_<:< right)
      (80, Map())
    else {
      // println("left = " + left + " " + left.getClass + " right = " + right + " " + right.getClass)

      left match {
        case TypeRef(leftTpe, leftSym, leftArgs) => {
          right match {
            case TypeRef(rightTpe, rightSym, rightArgs) => {
              val typeMap = collection.mutable.Map[Symbol, Type]()

              // println("leftSym = " + leftTpe + " " + leftSym + " " + leftArgs + " right = " + rightTpe + " " + rightSym + " " + rightArgs)
              var r =
                if (leftSym.typeSignature =:= rightSym.typeSignature) // Outer type is matched fully
                  50
                else
                if (leftSym.typeSignature <:< rightSym.typeSignature) // Outer type inherits
                  30
                else
                if (rightTpe == NoPrefix) {
                  // Right symbol is generic type parameter
                  typeMap += rightSym -> left
                  20
                }
                else
                  0

              if (r > 0) {
                // now check generic type args
                // println("Checking generic type args of : " + left + "("+leftTpe+") " + right + "("+rightTpe+")")
                if (leftArgs.size == rightArgs.size) {
                  for (i <- 0 until leftArgs.size) {
                    val lefT = leftArgs(i)
                    val rightT = rightArgs(i)
                    val tR = compareTypes(lefT, rightT)
                    if (tR._1 != 0) {
                      typeMap ++= tR._2
                    }
                    else
                      r = 0
                  }
                }
              }
              (r, typeMap.toMap)
            }
            case _ => (0, Map())
          }
        }
        case _ => (0, Map())
      }
    }
  }

  protected def extractAdders[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("add") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramss.nonEmpty &&
          (m.paramss.tail.isEmpty || allImplicits(m.paramss.tail)) &&
          m.paramss.head.size == 1 // only 1 parameter
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def extractSetters[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("set") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramss.nonEmpty &&
          (m.paramss.tail.isEmpty || allImplicits(m.paramss.tail)) &&
          m.paramss.head.size == 2 && // only 2 parameters
          m.paramss.head(0).typeSignature =:= typeOf[String]
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def findGetter(getters: List[MethodSymbol], returnType: Type): (Option[MethodSymbol], Map[Symbol, Type]) = {
    mostMatching(getters, m => {
      Some(compareTypes(returnType, m.returnType))
    })
  }

  private def extractGetters[T: c.WeakTypeTag]: List[MethodSymbol] = {

    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("get") &&
      member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      // println(m.paramss)
      m.paramss.nonEmpty &&
        (m.paramss.tail.isEmpty || allImplicits(m.paramss.tail)) && // 1 group of parameters or 2 group with all implicits
        m.paramss.head.size == 1 && // only 1 parSym
        m.paramss.head(0).typeSignature =:= typeOf[String]
    }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def extractCasters[T: c.WeakTypeTag]: List[MethodSymbol] = {

    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("getAs") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        // println(m.paramss)
        m.paramss.isEmpty || (m.paramss.size == 1 && allImplicits(List(m.paramss.head)))
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def allImplicits(symbols: List[List[Symbol]]): Boolean = symbols.flatten.filter(!_.isImplicit).isEmpty

  protected def extractCaseClassParams[T: c.WeakTypeTag]: List[c.Symbol] = {

    val companioned = weakTypeOf[T].typeSymbol
    val companionSymbol = companioned.companionSymbol
    val companionType = companionSymbol.typeSignature

    companionType.declaration(newTermName("unapply")) match {
      case NoSymbol => c.abort(c.enclosingPosition, s"No setter or unapply function found for ${companioned.fullName}")
      case s =>
        val unapply = s.asMethod
        val unapplyReturnTypes = unapply.returnType match {
          case TypeRef(_, _, Nil) =>
            c.abort(c.enclosingPosition, s"Apply of ${companionSymbol} has no parameters. Are you using an empty case class?")
          case TypeRef(_, _, args) =>
            args.head match {
              case t@TypeRef(_, _, Nil) => Some(List(t))
              case t@TypeRef(_, _, args) =>
                if (t <:< typeOf[Option[_]]) Some(List(t))
                else if (t <:< typeOf[Seq[_]]) Some(List(t))
                else if (t <:< typeOf[Set[_]]) Some(List(t))
                else if (t <:< typeOf[Map[_, _]]) Some(List(t))
                else if (t <:< typeOf[Product]) Some(args)
              case _ => None
            }
          case _ => None
        }

        companionType.declaration(newTermName("apply")) match {
          case NoSymbol => c.abort(c.enclosingPosition, "No apply function found")
          case s =>
            // searches apply method corresponding to unapply
            val applies = s.asMethod.alternatives
            val apply = applies.collectFirst {
              case (apply: MethodSymbol) if (apply.paramss.headOption.map(_.map(_.asTerm.typeSignature)) == unapplyReturnTypes) => apply
            }
            apply match {
              case Some(apply) =>

                // println("apply found:" + apply)
                if (!apply.paramss.tail.isEmpty)
                  c.abort(c.enclosingPosition, "Couldn't use apply method with more than a single parameter group")

                apply.paramss.head

              case None => c.abort(c.enclosingPosition, "No apply function found matching unapply parameters")
            }
        }

    }
  }

  protected def parameterLiteral(symbol: c.Symbol, converter: Option[Converter]): Literal = {
    Literal(Constant(
      converter.map {
        _.convert(symbol.name.decoded)
      } getOrElse {
        symbol.name.decoded
      }
    ))
  }

  protected def findConverter[T: c.WeakTypeTag]: Option[Converter] = {
    val tpe = weakTypeOf[T]
    val converterTypeName = newTypeName("nameConverterType")

    val converterTypeOption = tpe.baseClasses.map {
      baseSymbol =>
        val baseType = tpe.baseType(baseSymbol)
        val ct = baseType.declaration(converterTypeName)
        ct match {
          case NoSymbol => None
          case _ =>
            val t = ct.typeSignature.asSeenFrom(tpe, baseSymbol)
            t.baseClasses.find(t.typeSymbol.isClass && _ == typeOf[Converter].typeSymbol).map { x =>
              t.typeSymbol.asClass
            } orElse {
              c.abort(c.enclosingPosition, s"$tpe.nameConverterType: ${t} is not a valid Converter, please use PlainConverter if you don't need convert identifier names")
            }
        }
    }.flatten.headOption

    converterTypeOption map { t =>
      // this is synchronized because of bug in scala
      // http://stackoverflow.com/questions/7826822/why-this-synchronized-instead-of-just-synchronized-in-scala
      this.synchronized {
        val ru = scala.reflect.runtime.universe

        // todo: there should a better way to get runtime-symbol from compile-time
        val clz = Class.forName(t.fullName)
        val mirror = ru.runtimeMirror(getClass.getClassLoader)
        val sym = mirror.classSymbol(clz)
        val r = mirror.reflectClass(sym)
        val m = r.symbol.typeSignature.member(ru.nme.CONSTRUCTOR).asMethod
        val ctr = r.reflectConstructor(m)
        ctr().asInstanceOf[Converter]
      }
    }
  }
}
