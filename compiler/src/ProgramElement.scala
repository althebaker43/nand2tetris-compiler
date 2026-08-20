
package compiler

import java.io.PrintWriter

case class CodeSymbol( val name : String, val symType : String, val kind : String, val index : Int)

class SymbolTable(val map : Map[String, CodeSymbol], val numStatic : Int = 0, val numField : Int = 0, val numArg : Int = 0, val numVar : Int = 0):

    def addStaticSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "static", numStatic))
        SymbolTable(newMap, numStatic+1, numField, numArg, numVar)

    def addFieldSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "field", numField))
        SymbolTable(newMap, numStatic, numField+1, numArg, numVar)

    def addArgSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "argument", numArg))
        SymbolTable(newMap, numStatic, numField, numArg+1, numVar)

    def addVarSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "variable", numVar))
        SymbolTable(newMap, numStatic, numField, numArg, numVar+1)

class CodeGeneratorState( val className : String, val classSymTable : SymbolTable, val subSymTable : SymbolTable, val lines : List[String])

class ProgramElement( val children : List[ProgramElement], val xmlTagName : String ):

    def writeXML( writer : PrintWriter, indent : String = "" ) : Unit =
        writer.println(indent + "<" + xmlTagName + ">")
        for child <- children do
            child.writeXML(writer, indent + "  ")
        writer.println(indent + "</" + xmlTagName + ">")

    def generateCode( state : CodeGeneratorState ) : CodeGeneratorState =
        state

    def getNumVars( remainingChildren : List[ProgramElement], numVars : Int = 0 ) : Int = 0

    def generateChildCode(state : CodeGeneratorState, remainingChildren : List[ProgramElement]) : CodeGeneratorState =
        if remainingChildren.isEmpty then
            state
        else
            generateChildCode(remainingChildren.head.generateCode(state), remainingChildren.tail)

case class ClassElement( override val children : List[ProgramElement] ) extends ProgramElement(children, "class"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState = generateChildCode(state, children)

case class ClassVarDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "classVarDec")

case class SubroutineDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineDec"):

    override def generateCode(state : CodeGeneratorState): CodeGeneratorState =
        val isMethod = children.head match
            case KeywordToken(kw) => kw == "method"
            case _ => false
        val isVoidReturn = children.drop(1).head match
            case KeywordToken(kw) => kw == "void"
            case _ => false
        val numVars = children.map(child => child.getNumVars(child.children)).reduce(_ + _)
        val subSymTable = if isMethod then
            SymbolTable(Map("this" -> CodeSymbol("this", state.className, "var", 0)), numVar=1)
        else
            SymbolTable(Map[String, CodeSymbol]())
        // Drop the sub kind (method or function) and return type to get the name
        children.drop(2).head match
            case IDToken(id) =>
                val funcCmds = List("function " + state.className + "." + id + " " + numVars.toString())
                val thisCmds = if isMethod then List("push argument 0", "pop pointer 0") else Nil
                val childState = generateChildCode(CodeGeneratorState(state.className, state.classSymTable, subSymTable, state.lines ++ funcCmds ++ thisCmds), children)
                CodeGeneratorState(state.className, state.classSymTable, subSymTable, childState.lines)
            case _ => state

case class ParameterList( override val children : List[ProgramElement] ) extends ProgramElement(children, "parameterList"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState = addParameterSymbols(state, children)

    def addParameterSymbols(state : CodeGeneratorState, remainingChildren : List[ProgramElement]) : CodeGeneratorState =
        if remainingChildren.isEmpty then
            state
        else
            var paramPair = remainingChildren.take(2)
            var paramTypeToken = paramPair.head
            var paramNameToken = paramPair.last
            (paramTypeToken, paramNameToken) match
                case (IDToken(paramType), IDToken(paramName)) =>
                    addParameterSymbols(CodeGeneratorState(state.className, state.classSymTable, state.subSymTable.addArgSymbol(paramName, paramType), state.lines), remainingChildren.drop(2))
                case (KeywordToken(paramType), IDToken(paramName)) =>
                    addParameterSymbols(CodeGeneratorState(state.className, state.classSymTable, state.subSymTable.addArgSymbol(paramName, paramType), state.lines), remainingChildren.drop(2))
                case _ =>
                    addParameterSymbols(state, remainingChildren.tail)

case class SubroutineBody( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineBody"):

    override def getNumVars( remainingChildren : List[ProgramElement], numVars : Int = 0 ) : Int =
        if remainingChildren.isEmpty then
            numVars
        else
            remainingChildren.head match
                case varDec : VarDec => getNumVars(remainingChildren.tail, numVars+1)
                case _ => getNumVars(remainingChildren.tail, numVars)

case class VarDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "varDec"):

    override def generateCode(state: CodeGeneratorState) : CodeGeneratorState =
        val varTypeToken = children.tail.head
        varTypeToken match
            case IDToken(id) =>
                addVarSymbols(state, id, children.drop(2))
            case KeywordToken(kw) =>
                addVarSymbols(state, kw, children.drop(2))
            case _ => state

    def addVarSymbols( state : CodeGeneratorState, varType : String, remainingChildren : List[ProgramElement] ) : CodeGeneratorState =
        if remainingChildren.isEmpty then
            state
        else
            remainingChildren.head match
                case IDToken(id) =>
                    val newState = CodeGeneratorState(state.className, state.classSymTable, state.subSymTable.addVarSymbol(id, varType), state.lines)
                    addVarSymbols(newState, varType, remainingChildren.tail)
                case _ => addVarSymbols(state, varType, remainingChildren.tail)

case class StatementList( override val children : List[ProgramElement] ) extends ProgramElement(children, "statements"):

    override def generateCode(state : CodeGeneratorState) : CodeGeneratorState = generateChildCode(state, children)

case class LetStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "letStatement")
case class IfStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "ifStatement")
case class WhileStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "whileStatement")
case class DoStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "doStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, state.lines ++ List("pop temp 0"))

case class ReturnStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "returnStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        val voidCmds = if children.length == 2 then List("push constant 0") else Nil
        CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, childState.lines ++ voidCmds ++ List("return"))

case class Expression( override val children : List[ProgramElement], lastSym : SymbolToken ) extends ProgramElement(children, "expression"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        children.head match
            case SymbolToken('-') => CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, childState.lines ++ List("neg"))
            case _ => generateOperatorCode(childState, children)

    def generateOperatorCode(state: CodeGeneratorState, remainingChildren : List[ProgramElement]): CodeGeneratorState =
        if remainingChildren.isEmpty then
            state
        else
            val opCmds = remainingChildren.head match
                case SymbolToken('+') => List("add")
                case SymbolToken('-') => List("sub")
                case SymbolToken('&') => List("and")
                case SymbolToken('|') => List("or")
                case SymbolToken('~') => List("not")
                case SymbolToken('<') => List("lt")
                case SymbolToken('>') => List("gt")
                case SymbolToken('=') => List("eq")
                case SymbolToken('*') => List("call Math.multiply")
                case SymbolToken('/') => List("call Math.divide")
                case _ => Nil
            CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, state.lines ++ opCmds)
            
case class ExpressionTerm( override val children : List[ProgramElement] ) extends ProgramElement(children, "term")

case class ExpressionList( override val children : List[ProgramElement] ) extends ProgramElement(children, "expressionList"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState = generateChildCode(state, children)
