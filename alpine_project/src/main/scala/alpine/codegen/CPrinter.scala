package alpine
package codegen

import alpine.ast
import alpine.ast.{RecordPattern, Wildcard}
import alpine.ast.Typecast.{Narrow, NarrowUnconditionally, Widen}
import alpine.symbols
import alpine.symbols.Entity.builtinModule
import alpine.util.FatalError

import scala.annotation.tailrec
import scala.collection.mutable
import alpine.symbols.Type
import alpine.symbols.Type.Bool

/** The transpilation of an Alpine program to C. */
final class CPrinter(syntax: TypedProgram) extends ast.TreeVisitor[CPrinter.Context, Unit]:

  import CPrinter.Context

  /** The program being evaluated. */
  private given TypedProgram = syntax

  private def addCLibrary(using context: Context): Unit =
    context.output ++= "#include <stdio.h>\n"
    context.output ++= "#include <string.h>\n"
    context.output ++= "#include <stdlib.h>\n"
    context.output ++= "#include  <stdbool.h>\n"
    context.output ++= "#include \"builtin.h\"\n\n"
    context.output ++= "//RECORD DECLARATION\n"
    context.output ++= "//PATTERN\n\n"
    context.output ++= "\n//MAIN\n"
    context.output ++= "\n\n"


  private def addPatternMatcher(using context: Context): Unit =
    val recordTypes = context.typesToEmit.toList
    val indexPattern = context.output.lastIndexOf("//PATTERN\n") + "//PATTERN\n".length

    context.output.insert(indexPattern,"typedef struct {\n" +
      "  PatternType payload;\n" +
      "  int discriminator;\n" +
      "} PatternMatcher;\n\n")
    context.output.insert(indexPattern,"typedef union {\n" +
      "} PatternType;\n\n")

    val indexUnion =  context.output.lastIndexOf("typedef union {\n") + "typedef union {\n".length
    val sb = StringBuilder()
    context.typesToEmit.foreach( t =>
      sb ++= s"  ${transpiledType(t)} ${transpiledType(t).toLowerCase};\n"
    )
    context.output.insert(indexUnion, sb.toString())

    for(t <- recordTypes){
      if (context.nbPattern == 0) {
        context.output.insert(indexPattern, s"#define ${transpiledType(t).toUpperCase()} ${context.nbPattern}\n\n")
      }else{
        context.output.insert(indexPattern, s"#define ${transpiledType(t).toUpperCase()} ${context.nbPattern}\n")
      }

      context.nbPattern += 1
    }
  /** Returns a Scala program equivalent to `syntax`. */
  def transpile(): String =
    val c: Context = Context()
    addCLibrary(using c)
    for(syntax <- syntax.declarations){
      syntax.visit(this)(using c)
    }
    c.typesToEmit.toList.reverse.foreach(rec => emitRecord(rec)(using c))
    addPatternMatcher(using c)
    c.output.toString

  /** Writes the Scala declaration of `t` in `context`. */
  private def emitRecord(t: symbols.Type.Record)(using context: Context): Unit =
    if(t.fields.isEmpty){
      context.output ++= s"typedef struct {} ${transpiledType(t)};\n\n"
    }else{
      emitNonSingletonRecord(t)
    }



  /** Writes the Scala declaration of `t`, which is not a singleton, in `context`. */
  private def emitNonSingletonRecord(t: symbols.Type.Record)(using context: Context): Unit =
    val fields = t.fields.zipWithIndex.map { case (field, index) =>
      val label = field.label.getOrElse(" $"+ s"$index")
      s"${transpiledType(field.value)}$label"
    }
    val recordDeclaration = StringBuilder()
    recordDeclaration ++= s"typedef struct {\n"
    context.indentation += 1
    fields.foreach(field =>
    recordDeclaration ++= "  " * context.indentation + field + ";\n")
    context.indentation -= 1
    recordDeclaration ++= s"} ${transpiledRecord(t)};\n\n"

    val index = context.output.lastIndexOf("//RECORD DECLARATION\n") + "//RECORD DECLARATION\n".length
    context.output.insert(
      index,
      recordDeclaration.toString())


  /** Returns the transpiled form of `t`. */
  private def transpiledType(t: symbols.Type)(using context: Context): String =
    t match
      case u: symbols.Type.Builtin =>
        transpiledBuiltin(u)
      case u: symbols.Type.Record =>
        transpiledRecord(u)
      case u: symbols.Type.Arrow =>
        transpiledArrow(u)
      case u: symbols.Type.Sum =>
        transpiledSum(u)
      case _ => throw Error(s"type '${t}' is not representable in Scala")

  /** Returns the transpiled form of `t`. */
  private def transpiledBuiltin(t: symbols.Type.Builtin)(using context: Context): String =
    t match
      case symbols.Type.BuiltinModule => throw Error(s"type '${t}' is not representable in C")
      case symbols.Type.Bool => "bool "
      case symbols.Type.Int => "int "
      case symbols.Type.Float => "float "
      case symbols.Type.String => "char* "
      case symbols.Type.Any => "void* "

  /** Returns the transpiled form of `t`. */
  private def transpiledRecord(t: symbols.Type.Record)(using context: Context): String =
    if t == symbols.Type.Unit then
      "void"
    else
      context.registerUse(t)
      val d = if t.identifier == "#some" then "Some" else discriminator(t)
      if t.fields.isEmpty then s"${d}_None" else d

  /** Returns the transpiled form of `t`. */
  private def transpiledArrow(t: symbols.Type.Arrow)(using context: Context): String =
    val r = StringBuilder()
    r ++= "("
    r.appendCommaSeparated(t.inputs) { (o, a) => o ++= transpiledType(a.value) }
    r ++= " => "
    r ++= transpiledType(t.output)
    r ++= ")"
    r.toString()

  /** Returns the transpiled form of `t`. */
  private def transpiledSum(t: symbols.Type.Sum)(using context: Context): String =
    if t.members.isEmpty then "N" else
      t.members.map(transpiledType).mkString(" | ")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type): String =
    t match
      case u: symbols.Type.Builtin =>
        discriminator(u)
      case u: symbols.Type.Meta =>
        s"M${discriminator(u.instance)}"
      case u: symbols.Type.Definition =>
        "D" + u.identifier
      case u: symbols.Type.Record =>
        discriminator(u)
      case u: symbols.Type.Arrow =>
        discriminator(u)
      case u: symbols.Type.Sum =>
        discriminator(u)
      case _ =>
        throw Error(s"unexpected type '${t}'")

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Builtin): String =
    t match
      case symbols.Type.BuiltinModule => "Z"
      case symbols.Type.Bool => "B"
      case symbols.Type.Int => "I"
      case symbols.Type.Float => "F"
      case symbols.Type.String => "S"
      case symbols.Type.Any => "A"

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Record): String =
    val b = StringBuilder("Rec")
    for i <- t.fields do
      b++= "_"
      b++= i.label.getOrElse("")
      b++= discriminator(i.value)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Arrow): String =
    val b = StringBuilder("X")
    for i <- t.inputs do
      b ++= i.label.getOrElse("")
      b ++= discriminator(i.value)
    b ++= discriminator(t.output)
    b.toString

  /** Returns a string uniquely identifiyng `t` for use as a discriminator in a mangled name. */
  private def discriminator(t: symbols.Type.Sum): String =
    if t.members.isEmpty then "N" else
      "E" + t.members.map(discriminator).mkString

  /** Returns a transpiled reference to `e`. */
  private def transpiledReferenceTo(e: symbols.Entity): String =
    e match
      case symbols.Entity.Builtin(n, _) => n.identifier.toUpperCase()
      case symbols.Entity.Declaration(n, t) => scalaized(n) + discriminator(t)
      case symbols.Entity.Field(r,index) => discriminator(r.fields(index).value)

  /** Returns a string representation of `n` suitable for use as a Scala identifier. */
  private def scalaized(n: symbols.Name): String =
    n.qualification match
      case Some(q) =>
        s"${scalaized(q)}_${n.identifier}"
      case None =>
        "_" + n.identifier

  override def visitLabeled[T <: ast.Tree](n: ast.Labeled[T])(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitBinding(n: ast.Binding)(using context: Context): Unit =
    // Bindings represent global symbols at top-level.

    if context.isTopLevel then
      context.output ++= "  " * context.indentation

      // If the is the entry point if it's called "main".
      if n.identifier == "main" then
        context.output.insert(
          context.output.lastIndexOf("//MAIN\n") + "//MAIN\n".length,
          "int main(){")
      else
        if(!n.entityDeclared.tpe.isSubtypeOf(Type.Unit)){
          context.output ++= "  " * (context.indentation + 1) + transpiledType(n.tpe) + " "
          context.output ++= transpiledReferenceTo(n.entityDeclared)
        }


      // Top-level bindings must have an initializer.

      assert(n.initializer.isDefined)
      context.indentation += 1
      if( !n.entityDeclared.tpe.isSubtypeOf(Type.Unit) ){
        context.output ++= " ="
      }

      context.output ++= "  " * context.indentation
      n.initializer.get match
        case ast.Identifier(value,_) if value != "print" =>
        case _ =>
          context.inScope((c) => n.initializer.get.visit(this)(using c))
          context.output ++= ";\n\n"

      if (n.identifier == "main") {
        context.output ++= "  " * context.indentation + "return 0;\n"
        context.output ++= "}"
      }
      context.indentation -= 1

    // Bindings at local-scope are used in let-bindings and pattern cases.
    else
      context.output ++= "  " * context.indentation + s"case ${transpiledType(n.tpe).toUpperCase()}:\n"

  override def visitTypeDeclaration(n: ast.TypeDeclaration)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitFunction(n: ast.Function)(using context: Context): Unit =
    context.output ++= "  " * context.indentation


    context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output)

    context.output ++= transpiledReferenceTo(n.entityDeclared)
    context.output ++= "("
    context.output.appendCommaSeparated(n.inputs) { (o, a) =>
      o ++= transpiledType(a.tpe)
      o ++= " "
      o ++= a.identifier
    }
    context.output ++= "){\n "
    context.indentation += 1
    context.output ++= "  " * context.indentation
    context.inScope((c) => n.body.visit(this)(using c))
    // add return statement to the function
    context.output.insert(
      context.output.lastIndexOf(" ")+1,
      "return ")
    context.output ++= ";\n\n"
    context.indentation -= 1
    context.output ++= "}\n"

  override def visitParameter(n: ast.Parameter)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitIdentifier(n: ast.Identifier)(using context: Context): Unit =
    if(context.isTopLevel){
      context.output ++= transpiledReferenceTo(n.referredEntity.get.entity)
    }else{
      context.output ++= context.patternBindings.getOrElse(n.value,transpiledReferenceTo(n.referredEntity.get.entity))
    }

  override def visitBooleanLiteral(n: ast.BooleanLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString

  override def visitIntegerLiteral(n: ast.IntegerLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString

  override def visitFloatLiteral(n: ast.FloatLiteral)(using context: Context): Unit =
    context.output ++= n.value.toString ++ "f"

  override def visitStringLiteral(n: ast.StringLiteral)(using context: Context): Unit =
    context.output ++= n.value

  override def visitRecord(n: ast.Record)(using context: Context): Unit =
    val t = n.tpe.asInstanceOf[symbols.Type.Record]
    context.registerUse(t)
    context.output ++= "{"
    context.output.appendCommaSeparated(n.fields) { (o, a) => a.value.visit(this) }
    context.output ++= "}"


  override def visitSelection(n: ast.Selection)(using context: Context): Unit =
      if(context.isTopLevel){
        n.qualification.visit(this)
      }
      n.referredEntity match
        case Some(symbols.EntityReference(e: symbols.Entity.Field, _)) =>
          if(context.isTopLevel){
            context.output ++= "." + e.whole.fields(e.index).label.getOrElse("$" + s"${e.index}")
          }else{
            context.output ++= "p.payload." +
              transpiledType(e.whole).toLowerCase() + "."
              + e.whole.fields(e.index).label.getOrElse("$" + s"${e.index}")
          }
        case _ =>
          unexpectedVisit(n.selectee)


  override def visitApplication(n: ast.Application)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    context.output.appendCommaSeparated(n.arguments) { (o, a) => a.value.visit(this) }
    context.output ++= ")"

  override def visitPrefixApplication(n: ast.PrefixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.argument.visit(this)
    context.output ++= ")"

  override def visitInfixApplication(n: ast.InfixApplication)(using context: Context): Unit =
    n.function.visit(this)
    context.output ++= "("
    n.lhs.visit(this)
    context.output ++= ", "
    n.rhs.visit(this)
    context.output ++= ")"

  override def visitConditional(n: ast.Conditional)(using context: Context): Unit =
    n.condition.visit(this)
    context.output ++= " ? "
    n.successCase.visit(this)
    context.output ++= " : "
    n.failureCase.visit(this)

  override def visitMatch(n: ast.Match)(using context: Context): Unit =
    context.patternBindings.clear()
    context.output ++= "match((PatternMatcher){"
    context.output ++= s".payload.${transpiledType(n.scrutinee.tpe).toLowerCase} = "
    n.scrutinee.visit(this)
    context.output ++= s", ${transpiledType(n.scrutinee.tpe).toUpperCase()}})"

    val currentContext = context.output.clone()
    context.output.clear()

    context.output ++= s"${transpiledType(n.cases.head.body.tpe)} match(PatternMatcher p){\n"
    context.indentation += 1
    context.output ++= "  " * context.indentation + "switch(p.discriminator){\n"
    context.indentation += 1

    for mc <- n.cases do
      visitMatchCase(mc)
    context.indentation -= 1
    context.output ++= "  " * context.indentation + "}\n"
    context.indentation -= 1
    context.output ++= "  " * context.indentation + "}\n\n"

    val matchFunction = context.output.toString()
    context.output.clear()
    context.output ++= currentContext
    context.output.insert(context.output.lastIndexOf("//PATTERN\n") + "//PATTERN\n".length,matchFunction)


  override def visitMatchCase(n: ast.Match.Case)(using context: Context): Unit =
    n.pattern match
      case recordPattern :ast.RecordPattern =>
        context.output ++= "  " * context.indentation + s"case ${transpiledType(n.pattern.tpe).toUpperCase()}:\n"
      case ast.Wildcard(_) =>
        context.output ++= "  " * context.indentation + s"default:\n"
      case _ =>
    n.pattern.visit(this)
    context.indentation += 1
    context.output ++= "  " * context.indentation
    n.body.tpe match
      case Type.Unit => 
      case _ => context.output ++= "return "

    n.body.visit(this)
    context.output ++= ";\n"
    context.output ++=  "  " * context.indentation + "break;\n"
    context.indentation -= 1


  override def visitLet(n: ast.Let)(using context: Context): Unit =
    // Use a block to uphold lexical scoping.
    context.output ++= "{\n"
    context.indentation += 1
    context.output ++= "  " * context.indentation
    n.binding.visit(this)
    context.output ++= "\n"
    context.output ++= "  " * context.indentation
    n.body.visit(this)
    context.output ++= "\n"
    context.indentation -= 1
    context.output ++= "  " * context.indentation
    context.output ++= "}"

  override def visitLambda(n: ast.Lambda)(using context: Context): Unit =
    context.output ++= "("
    context.output.appendCommaSeparated(n.inputs) { (o, a) =>
      o ++= a.identifier
      o ++= ": "
      o ++= transpiledType(a.tpe)
    }
    context.output ++= ") => ("
    n.body.visit(this)
    context.output ++= "): "
    context.output ++= transpiledType(symbols.Type.Arrow.from(n.tpe).get.output)

  override def visitParenthesizedExpression(
                                             n: ast.ParenthesizedExpression
                                           )(using context: Context): Unit =
    context.output ++= "("
    n.inner.visit(this)
    context.output ++= ")"

  override def visitAscribedExpression(
                                        n: ast.AscribedExpression
                                      )(using context: Context): Unit =
    n.operation match
      case Widen =>
        n.inner.visit(this)
        context.output ++= s".asInstanceOf[${transpiledType(n.ascription.tpe)}]"
      case NarrowUnconditionally =>
        context.output ++= "try {\n"
        context.output ++= "  " * context.indentation
        n.inner.visit(this)
        context.output ++= s".asInstanceOf[${transpiledType(n.ascription.tpe)}]"
        context.output ++= "\n"
        context.output ++= "} catch { "
        context.output ++= "  " * context.indentation
        context.output ++= "case e : ClassCastException => \"panic\" }"

      case Narrow =>
        context.output ++=  "if ("
        n.inner.visit(this)
        context.output ++= s".isInstanceOf[${transpiledType(n.ascription.tpe)}]"
        context.output ++= ") "
        context.output ++= "Some("
        n.inner.visit(this)
        context.output ++= s".asInstanceOf[${transpiledType(n.ascription.tpe)}]"
        context.output ++= ")"
        context.output ++= " else "
        context.output ++= "Rec_None"




  override def visitTypeIdentifier(n: ast.TypeIdentifier)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitRecordType(n: ast.RecordType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitTypeApplication(n: ast.TypeApplication)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitArrow(n: ast.Arrow)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitSum(n: ast.Sum)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitParenthesizedType(n: ast.ParenthesizedType)(using context: Context): Unit =
    unexpectedVisit(n)

  override def visitValuePattern(n: ast.ValuePattern)(using context: Context): Unit =
    {}

  private def capturePatternBinding(n: ast.RecordPattern, captureName: String)(using context: Context): Unit =
    for ((field,index) <- n.fields.zipWithIndex) {
      val selectee = s".${field.label.getOrElse("$" + s"${index}")}"
      val newCapture = captureName + selectee
        field.value match
        case recordPattern: RecordPattern  =>
          capturePatternBinding(recordPattern,newCapture)
        case binding: ast.Binding =>
          context.patternBindings.addOne(
            binding.identifier,
            newCapture
          )
        case _ =>
    }
  override def visitRecordPattern(n: ast.RecordPattern)(using context: Context): Unit =
    capturePatternBinding(n, s"p.payload.${transpiledType(n.tpe).toLowerCase}")(using context)

  override def visitWildcard(n: ast.Wildcard)(using context: Context): Unit =
    {}

  override def visitError(n: ast.ErrorTree)(using context: Context): Unit =
    unexpectedVisit(n)

object CPrinter:

  /** The local state of a transpilation to C.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var indentation: Int = 0,var nbPattern: Int = 0):

    /** The types that must be emitted in the program. */
    private var _typesToEmit = mutable.Set[symbols.Type.Record]()

    /** The types that must be emitted in the program. */
    def typesToEmit: Set[symbols.Type.Record] = _typesToEmit.toSet

    private val _patternBindings: mutable.Map[String,String] = mutable.Map[String,String]()

    def patternBindings: mutable.Map[String,String] = _patternBindings
    /** The (partial) result of the transpilation. */
    private var _output = StringBuilder()

    /** The (partial) result of the transpilation. */
    def output: StringBuilder = _output

    /** `true` iff the transpiler is processing top-level symbols. */
    private var _isTopLevel = true

    /** `true` iff the transpiler is processing top-level symbols. */
    def isTopLevel: Boolean = _isTopLevel

    /** Adds `t` to the set of types that are used by the transpiled program. */
    def registerUse(t: symbols.Type.Record): Unit =
      if t != symbols.Type.Unit then _typesToEmit.add(t)

    /** Returns `action` applied on `this` where `output` has been exchanged with `o`. */
    def swappingOutputBuffer[R](o: StringBuilder)(action: Context => R): R =
      val old = _output
      _output = o
      try action(this) finally _output = old

    /** Returns `action` applied on `this` where `isTopLevel` is `false`. */
    def inScope[R](action: Context => R): R =
      var tl = _isTopLevel
      _isTopLevel = false
      try action(this) finally _isTopLevel = tl

  end Context

end CPrinter


