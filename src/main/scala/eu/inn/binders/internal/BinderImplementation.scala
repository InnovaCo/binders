package eu.inn.internal

import scala.language.reflectiveCalls
import scala.reflect.macros.Context
import language.experimental.macros
import eu.inn.binders.naming.Converter

private trait BinderImplementation {
  val c: Context

  import c.universe._

  def bind[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val writers = extractWriters[S]
    val tpe = weakTypeOf[O]
    val writer = findWriter(writers, tpe)

    val block =
    if (!writer.isDefined) {
      if (tpe <:< typeOf[Option[_]]){
        bindOption[S, O](value)
      }
      else if (tpe <:< typeOf[Product]){
        bindObject[S, O](value, partial = false)
      }
      else if (tpe <:< typeOf[TraversableOnce[_]]){
        bindTraversable[S, O](value)
      }
      else
        c.abort(c.enclosingPosition, s"No write function found for parameter with type $tpe")
    }
    else {
      q"""{
        val t = ${c.prefix.tree}
        ${makeReaderWriterCall(q"t.serializer", writer.get, List(value))}
        t.serializer
        }"""
    }
    //println(block)
    block
  }

  def bindArgs[S: c.WeakTypeTag](args: Seq[c.Tree]): c.Tree = {
    val bindList = args.map(arg => q"ta.serializer.bind($arg)")
    val block = q"""{
      val ta = ${c.prefix.tree}
      ${callIfExists[S](q"ta.serializer", "beginArgs")}
      ..$bindList
      ${callIfExists[S](q"ta.serializer", "endArgs")}
      ta.serializer
      }"""

    // println(block)
    block
  }

  def bindOption[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val block = q"""{
      val to = ${c.prefix.tree}
      val o = $value
      o.map {
        ov => to.serializer.bind(ov)
      } getOrElse {
        to.serializer.writeNull
      }
      to.serializer.serializer
      }"""
    //println(block)
    block
  }

  def bindObject[S: c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree, partial: Boolean): c.Tree = {
    val converter = findConverter[S]
    val caseClassParams = extractCaseClassParams[O]

    val listOfCalls: List[Tree] = caseClassParams.map { parameter =>
      val fieldName = identToFieldName(parameter, converter)
      if (partial)
        q"tx.serializer.getFieldSerializer($fieldName).map(_.bind(o.${newTermName(parameter.name.toString)}))"
      else
        q"getFieldOrThrow(tx.serializer.getFieldSerializer($fieldName), $fieldName).bind(o.${newTermName(parameter.name.toString)})"
    }

    val block = q"""{
      import eu.inn.binders.internal.Helpers._
      val tx = ${c.prefix.tree}
      val o = $value
      ${callIfExists[S](q"tx.serializer", "beginObject")}
      ..$listOfCalls
      ${callIfExists[S](q"tx.serializer", "endObject")}
      tx.serializer
      }"""
    //println(block + " partial = " + partial)
    block
  }

  def bindTraversable[S : c.WeakTypeTag, O: c.WeakTypeTag](value: c.Tree): c.Tree = {
    val block = q"""{
      val ts = ${c.prefix.tree}
      val it = $value
      ${callIfExists[S](q"ts.serializer", "beginArray")}
      it.foreach(ts.bind(_))
      ${callIfExists[S](q"ts.serializer", "endArray")}
    }"""
    //println(block)
    block
  }

  def unbind[D: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {
    val tpe = weakTypeOf[O]
    val readers = extractReaders[D]
    //println ("readers: " + readers)
    val reader = findReader(readers, tpe)
    //println("reader: " + reader)
    val block =
      reader.map { g =>
        q"""{
          val tu = ${c.prefix.tree}
          ${makeReaderWriterCall(q"tu.deserializer", g)}
        }"""
      } getOrElse {
        val companionType = tpe.typeSymbol.companionSymbol.typeSignature
        companionType.declaration(newTermName("unapply")) match {
          case NoSymbol =>
            if (tpe <:< typeOf[Option[_]]) {
              unbindOption[D,O]
            }
            else if (tpe <:< typeOf[TraversableOnce[_]]) {
              unbindIterable[D,O]
            }
            else {
              c.abort(c.enclosingPosition, s"No read function found for $tpe in ${weakTypeOf[D]}")
            }
          case s => unbindObject[D,O](partial, originalValue)
        }
      }
    //println(block)
    block
  }

  def unbindOption[D: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head

    val block = q"""{
      val too = ${c.prefix.tree}
      if (too.deserializer.isNull)
        None
      else
        Some(too.deserializer.unbind[$elTpe])
    }"""
    //println(block)
    block
  }

  def unbindIterable[D: c.WeakTypeTag, O: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[O]
    val elTpe = extractTypeArgs(tpe).head

    q"""{
      val ti = ${c.prefix.tree}
      ${convertIterator(tpe, q"ti.deserializer.iterator().map(_.unbind[$elTpe])")}
    }"""
  }

  def unbindObject[D: c.WeakTypeTag, O: c.WeakTypeTag](partial: Boolean, originalValue: c.Tree): c.Tree = {
    val converter = findConverter[D]
    val caseClassParams = extractCaseClassParams[O]

    val vars = caseClassParams.map { parameter =>
      val varName = newTermName("i_" + parameter.name.decoded)
      val fieldName = identToFieldName(parameter, converter)
      (
        // _1
        if (partial)
          q"var $varName : Option[${parameter.typeSignature}] = Some(orig.${newTermName(parameter.name.decoded)})"
        else
          q"var $varName : Option[${parameter.typeSignature}] = None",

        // _2
        cq"""$fieldName => {
            val v = i.unbind[${parameter.typeSignature}]
            $varName = Some(v)
          }""",

        // _3
        if (parameter.typeSignature <:< typeOf[Option[_]])
          q"$varName.flatten"
        else
          q"$varName.getOrElse(throw new eu.inn.binders.core.FieldNotFoundException($fieldName))"
      )
    }

    val outputCompanionSymbol = weakTypeOf[O].typeSymbol.companionSymbol

    q"""{
      val tpi = ${c.prefix.tree}
      ${if (partial) { q"val orig = ${originalValue}" } else q""}
      ..${vars.map(_._1)}
      tpi.deserializer.iterator().foreach{i =>
        i.fieldName.map { fieldName =>
          fieldName match {
            case ..${vars.map(_._2)}
            case _ => { /*todo: implement smart deserialization*/ }
          }
        } getOrElse {
          throw new eu.inn.binders.core.BindersException("Iterator didn't return fieldName")
        }
      }
      ${outputCompanionSymbol}(
        ..${vars.map(_._3)}
      )
    }"""
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
      if (ct <:< typeOf[Set[_]]) {
        Some("toSet")
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

  protected def makeReaderWriterCall(elemTerm: Tree, method: (MethodSymbol, Map[Symbol, Type]), parameters: List[Tree] = List()): Apply = {
    val inner = Apply(applyTypeArgs(Select(elemTerm, method._1),  method._2,  method._1.typeParams), parameters)
    if (method._1.paramss.isEmpty)
      inner
    else
      method._1.paramss.tail.foldLeft(inner) { (a: Apply, params: List[Symbol]) =>
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

  protected def mostMatching(methods: List[MethodSymbol], scoreFun: MethodSymbol => Option[(Int, Map[Symbol, Type])]): Option[(MethodSymbol, Map[Symbol, Type])] = {
    var rMax: Int = 0
    var mRes: Option[(MethodSymbol,Map[Symbol, Type])] = None
    methods.map({ m => // todo: replace to .max and remove vars
      scoreFun(m) match {
        case Some((r, typeArgs)) =>
          if (r > rMax) {
            rMax = r
            mRes = Some(m, typeArgs)
          }
        case None => // do nothing
      }
      // println("Comparing " + m + " with arg type " + methodParSym.typeSignature + " for parameter " + parSym + " with type " + parSymType + " RES = " + r + " --- " + math.random)
    })
    mRes
  }

  protected def findWriter(writers: List[MethodSymbol], valueType: Type/*, print: Boolean = false*/): Option[(MethodSymbol, Map[Symbol, Type])] = {
    //println("writers = " + writers)
    mostMatching(writers, m => {
      val writerType = m.paramss.head(0) // parSym 0 - value
      Some(compareTypes(valueType, writerType.typeSignature/*, print*/))
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
      left match {
        case TypeRef(_, _, leftArgs) => {
          right match {
            case TypeRef(rightTpe, rightSym, rightArgs) => {
              val typeMap = collection.mutable.Map[Symbol, Type]()

              // println(s"lt = ${left.baseClasses} rt = ${rightTpe.baseClasses}")
              var r =
                if (left.typeSymbol.typeSignature =:= right.typeSymbol.typeSignature) // Outer type is matched fully
                  50
                else
                if (left.baseClasses.exists(_.typeSignature =:= right.typeSymbol.typeSignature)) // Outer type inherits
                  30
                else
                if (rightTpe == NoPrefix) {
                  // Right symbol is generic type parameter
                  typeMap += rightSym -> left
                  20
                }
                else
                  0

              //println("LB = " + left.baseClasses)
              //println("leftSym = " + left + " | " + leftArgs + " right = " + right + " | " + rightArgs + " r = " + r)

              if (r > 0) {
                // now check generic type args
                // println("Checking generic type args of : " + left + "("+leftTpe+") " + right + "("+rightTpe+") r = " + r)
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

  protected def callIfExists[S: c.WeakTypeTag](o: c.Tree, methodName: String): c.Tree = {
    weakTypeOf[S].members.filter(member => member.isMethod &&
      member.name.decoded == methodName &&
      member.isPublic && {
      val m = member.asInstanceOf[MethodSymbol]
      //println("method: " + member.name.decoded + " params: " + m.paramss)
      m.paramss.isEmpty ||
        (m.paramss.size == 1 && allImplicits(List(m.paramss.head))) ||
        (m.paramss.size == 2 && m.paramss.head.isEmpty && allImplicits(m.paramss.tail))
    }
    ).map(_.asInstanceOf[MethodSymbol]).headOption.map { m =>
      q"$o.${newTermName(methodName)}()"
    } getOrElse {
      q""
    }
  }

  protected def extractWriters[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("write") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        m.paramss.nonEmpty &&
          (m.paramss.tail.isEmpty || allImplicits(m.paramss.tail)) &&
          m.paramss.head.size == 1 // only 1 parameter
      }
    ).map(_.asInstanceOf[MethodSymbol]).toList
  }

  protected def findReader(readers: List[MethodSymbol], returnType: Type): Option[(MethodSymbol, Map[Symbol, Type])] = {
    mostMatching(readers, m => {
      Some(compareTypes(returnType, m.returnType))
    })
  }

  protected def extractReaders[T: c.WeakTypeTag]: List[MethodSymbol] = {
    weakTypeOf[T].members.filter(member => member.isMethod &&
      member.name.decoded.startsWith("read") &&
      member.isPublic && {
        val m = member.asInstanceOf[MethodSymbol]
        //println("method: " + member.name.decoded + " params: " + m.paramss)
        m.paramss.isEmpty ||
          (m.paramss.size == 1 && allImplicits(List(m.paramss.head))) ||
          (m.paramss.size == 2 && m.paramss.head.isEmpty && allImplicits(m.paramss.tail))
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

  protected def identToFieldName(symbol: c.Symbol, converter: Option[Converter]): Literal = {
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
