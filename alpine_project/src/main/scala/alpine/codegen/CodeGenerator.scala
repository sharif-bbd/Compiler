package alpine
package codegen

import alpine.symbols
import alpine.wasm.WasmTree.*
import alpine.ast.*
import alpine.wasm.Wasm

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** The transpilation of an Alpine program to Scala. */
final class CodeGenerator(syntax: TypedProgram) extends ast.TreeVisitor[CodeGenerator.Context, Unit]:
  import CodeGenerator._

  /** The program being evaluated. */
  private given TypedProgram = syntax

  /** Returns a WebAssembly program equivalent to `syntax`. */
  /** THIS IS AN EXAMPLE MODULE! */
  def compile(): Module =

    given c: Context = Context()

    for (declaration <- syntax.declarations) {
      declaration.visit(this)
    }
    val mainFunction = MainFunction(c.output.toList,None)
    val moduleFunctions = c.functionDefinitions.map(f => emitFunctionDeclaration(f))
    Module(
      List(
        ImportFromModule("api", "print", "print", List(I32), None),
        ImportFromModule("api", "print", "fprint", List(F32), None),
        ImportFromModule("api", "print-char", "print-char", List(I32), None),
        ImportFromModule("api", "show-memory", "show-memory", List(I32), None),
        ImportMemory("api", "mem", 100)
      ),
      moduleFunctions.toList :+ mainFunction
  )

  private def convertToWasmType(symbolType : symbols.Type): WasmType =
    val wasmType = symbolType match
      case symbols.Type.Int => I32
      case symbols.Type.Float => F32
    wasmType.asInstanceOf[WasmType]

  private def emitFunctionDeclaration(n : ast.Function)(using a: Context): FunctionDefinition = 
    a.output.clear()
    n.body.visit(this)(using a)
    val params =  n.inputs.map{ param =>
      convertToWasmType(param.tpe)
    }
    val returnType = convertToWasmType(n.output.get.tpe)
    FunctionDefinition(
      n.identifier,
      params,
      params,
      Some(returnType),
      a.output.toList
    )
    
    
  // Tree visitor methods

  /** Visits `n` with state `a`. */
  def visitLabeled[T <: Tree](n: Labeled[T])(using a: Context): Unit =
    n.value.visit(this)(using a)

  /** Visits `n` with state `a`. */
  def visitBinding(n: Binding)(using a: Context): Unit =
    if(n.initializer.isDefined){
      n.initializer.get match
        case ast.Record(_,_,_) =>
          val recordType = n.tpe.asInstanceOf[symbols.Type.Record]
          a.record.put(n.identifier, (a.memoryIndex, recordType.fields))
        case _ =>
    }
    n.initializer.get.visit(this)(using a)

  /** Visits `n` with state `a`. */
  def visitTypeDeclaration(n: TypeDeclaration)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitFunction(n: ast.Function)(using a: Context): Unit =
    a.functionDefinitions.addOne(n)

  /** Visits `n` with state `a`. */
  def visitParameter(n: Parameter)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitIdentifier(n: Identifier)(using a: Context): Unit =
    {}


  /** Visits `n` with state `a`. */
  def visitBooleanLiteral(n: BooleanLiteral)(using a: Context): Unit =
    a.output.addOne(
      IConst(if (n.value == "true") 1 else 0)
    )

  /** Visits `n` with state `a`. */
  def visitIntegerLiteral(n: IntegerLiteral)(using a: Context): Unit =
    a.output.addOne(IConst(n.value.toInt))

  /** Visits `n` with state `a`. */
  def visitFloatLiteral(n: FloatLiteral)(using a: Context): Unit =
    a.output.addOne(FConst(n.value.toFloat))

  /** Visits `n` with state `a`. */
  def visitStringLiteral(n: StringLiteral)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecord(n: Record)(using a: Context): Unit =
    for(labeledExpr <- n.fields){
      a.output.addOne(IConst(a.memoryIndex))
      labeledExpr.value.visit(this)(using a)
      labeledExpr.value.tpe match
        case symbols.Type.Int => a.output.addOne(IStore)
        case symbols.Type.Float => a.output.addOne(FStore)
      a.memoryIndex += 4;
    }



  /** Visits `n` with state `a`. */
  def visitSelection(n: Selection)(using a: Context): Unit =
    def loadFromMemory(indexMemory : Int,typeToLoad : symbols.Type) : Unit =
      a.output.addOne(IConst(indexMemory))
      convertToWasmType(typeToLoad) match
        case I32 => a.output.addOne(ILoad)
        case F32 => a.output.addOne(FLoad)
        case _ =>
    n.qualification match
      case ast.Identifier(identifier,_) =>
        val (recordIndex,recordFields) = a.record(identifier)
        n.selectee match
          case IntegerLiteral(value,site) =>
            val (field,fieldIndex) = recordFields.zipWithIndex.find(field => field._2 == value.toInt).get
            loadFromMemory(recordIndex + fieldIndex*4,field.value)
          case Identifier(value,site) =>
            val (field,fieldIndex) = recordFields.zipWithIndex.find(field => field._1.label.getOrElse("") == value).get
            loadFromMemory(recordIndex + fieldIndex*4,field.value)
      case _ =>


  /** Visits `n` with state `a`. */
  def visitApplication(n: Application)(using a: Context): Unit =
    val funIdentifier = n.function.referredEntity.get.entity.name.identifier
    n.arguments.foreach(labeledExpr => labeledExpr.value.visit(this)(using a))
    if(funIdentifier == "print"){
      a.output.last match
        case IConst(value) => a.output.addOne(Call("print"))
        case FConst(value) => a.output.addOne(Call("fprint"))
        case Call(f)  =>
          a.functionDefinitions.find(fun => fun.identifier == f).get.output.get.tpe match
            case symbols.Type.Int => a.output.addOne(Call("print"))
            case symbols.Type.Float => a.output.addOne(Call("fprint"))
        case ILoad =>
          a.output.addOne(Call("print"))
        case FLoad =>
          a.output.addOne(Call("fprint"))
        case IAdd =>
          a.output.addOne(Call("print"))
        case _ =>
    }else{
      a.output.addOne(Call(funIdentifier))
    }



  /** Visits `n` with state `a`. */
  def visitPrefixApplication(n: PrefixApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitInfixApplication(n: InfixApplication)(using a: Context): Unit =
    n.lhs.visit(this)(using a)
    n.rhs.visit(this)(using a)
    if(n.function.value == "=="){
      a.output.addOne(IEq)
    }
    if(n.function.value == "+"){
      a.output.addOne(IAdd)
    }


  /** Visits `n` with state `a`. */
  def visitConditional(n: Conditional)(using a: Context): Unit =
    val currentContext = a.output.clone()
    a.output.clear()
    n.successCase.visit(this)(using a)
    val successCaseInstructions = a.output.toList
    a.output.clear()
    n.failureCase.visit(this)(using a)
    val failureCaseInstructions= a.output.toList
    a.output.clear()
    a.output ++= currentContext
    n.condition.visit(this)(using a)
    a.output.addOne(
        If_void(
        successCaseInstructions,
        Some(failureCaseInstructions)
      )
    )

  /** Visits `n` with state `a`. */
  def visitMatch(n: Match)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitMatchCase(n: Match.Case)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitLet(n: Let)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitLambda(n: Lambda)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedExpression(n: ParenthesizedExpression)(using a: Context): Unit =
    n.inner.visit(this)(using a)

  /** Visits `n` with state `a`. */
  def visitAscribedExpression(n: AscribedExpression)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeIdentifier(n: TypeIdentifier)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecordType(n: RecordType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitTypeApplication(n: TypeApplication)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitArrow(n: Arrow)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitSum(n: Sum)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitParenthesizedType(n: ParenthesizedType)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitValuePattern(n: ValuePattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitRecordPattern(n: RecordPattern)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitWildcard(n: Wildcard)(using a: Context): Unit = ???

  /** Visits `n` with state `a`. */
  def visitError(n: ErrorTree)(using a: Context): Unit = ???

object CodeGenerator:

  /** The local state of a transpilation to Scala.
   *
   *  @param indentation The current identation to add before newlines.
   */
  final class Context(var memoryIndex: Int = 0):


    /** The (partial) result of the transpilation. */
    private var _output = ListBuffer[Instruction]()

    /** The (partial) result of the transpilation. */
    def output: ListBuffer[Instruction] = _output

    private var _functionsToDefine= ListBuffer[ast.Function]()
    def functionDefinitions : ListBuffer[ast.Function] = _functionsToDefine

    val _record: mutable.Map[String, (Int, List[symbols.Type.Labeled])] = mutable.Map[String, (Int, List[symbols.Type.Labeled])]()

    def record: mutable.Map[String, (Int, List[symbols.Type.Labeled])] = _record




  end Context

