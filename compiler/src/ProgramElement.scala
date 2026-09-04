
package compiler

import java.io.PrintWriter

case class CodeSymbol( val name : String, val symType : String, val kind : String, val index : Int)

case class SymbolTable(val map : Map[String, CodeSymbol], val numStatic : Int = 0, val numField : Int = 0, val numArg : Int = 0, val numVar : Int = 0):

    def addStaticSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "static", numStatic))
        SymbolTable(newMap, numStatic+1, numField, numArg, numVar)

    def addFieldSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "this", numField))
        SymbolTable(newMap, numStatic, numField+1, numArg, numVar)

    def addArgSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "argument", numArg))
        SymbolTable(newMap, numStatic, numField, numArg+1, numVar)

    def addVarSymbol( name : String, symType : String ) : SymbolTable =
        val newMap = map + (name -> CodeSymbol(name, symType, "local", numVar))
        SymbolTable(newMap, numStatic, numField, numArg, numVar+1)

case class CodeGeneratorState( val className : String, val classSymTable : SymbolTable, val subSymTable : SymbolTable, val lines : List[String])

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

    def getClassName = getIDNames(children).head

    def getIDNames(remainingChildren: List[ProgramElement], ids: List[String] = Nil): List[String] =
        if remainingChildren.isEmpty then
            return ids
        else
            val newIDs = remainingChildren.head match
                case IDToken(id) => List(id)
                case _ => Nil
            getIDNames(remainingChildren.tail, ids ++ newIDs)

case class ClassVarDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "classVarDec"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val isField = children.head match
            case KeywordToken("field") => true
            case _ => false
        val typeName = children.drop(1).head match
            case KeywordToken(kw) => kw
            case IDToken(id) => id
            case _ => "NA"
        val varNames = getVarNames(children.drop(2))
        updateSymbols(isField, typeName, varNames, state)

    def getVarNames(remainingChildren: List[ProgramElement], varNames: List[String] = Nil): List[String] =
        if remainingChildren.isEmpty then
            varNames
        else
            val newVarNames = remainingChildren.head match
                case IDToken(id) => List(id)
                case _ => Nil
            getVarNames(remainingChildren.tail, varNames ++ newVarNames)

    def updateSymbols(isField: Boolean, typeName: String, varNames: List[String], state: CodeGeneratorState): CodeGeneratorState =
        if varNames.isEmpty then
            state
        else
            val newState = if isField then
                state.copy(classSymTable = state.classSymTable.addFieldSymbol(varNames.head, typeName))
            else
                state.copy(classSymTable = state.classSymTable.addStaticSymbol(varNames.head, typeName))
            updateSymbols(isField, typeName, varNames.tail, newState)

case class SubroutineDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineDec"):

    override def generateCode(state : CodeGeneratorState): CodeGeneratorState =
        val isConstructor = children.head match
            case KeywordToken("constructor") => true
            case _ => false
        val isMethod = children.head match
            case KeywordToken("method") => true
            case _ => false
        val isVoidReturn = children.drop(1).head match
            case KeywordToken("void") => true
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
                val thisCmds = if isConstructor then
                    List("call Memory.alloc " + state.classSymTable.map.values.count(_.kind == "field").toString(), "pop pointer 0")
                else if isMethod then
                    List("push argument 0", "pop pointer 0")
                else
                    Nil
                val childState = generateChildCode(CodeGeneratorState(state.className, state.classSymTable, subSymTable, state.lines ++ funcCmds ++ thisCmds), children)
                state.copy(subSymTable = subSymTable, lines = childState.lines)
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
                    addParameterSymbols(state.copy(subSymTable = state.subSymTable.addArgSymbol(paramName, paramType)), remainingChildren.drop(2))
                case (KeywordToken(paramType), IDToken(paramName)) =>
                    // addParameterSymbols(CodeGeneratorState(state.className, state.classSymTable, state.subSymTable.addArgSymbol(paramName, paramType), state.lines), remainingChildren.drop(2))
                    addParameterSymbols(state.copy(subSymTable = state.subSymTable.addArgSymbol(paramName, paramType)), remainingChildren.drop(2))
                case _ =>
                    addParameterSymbols(state, remainingChildren.tail)

case class SubroutineBody( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineBody"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState = generateChildCode(state, children)

    override def getNumVars( remainingChildren : List[ProgramElement], numVars : Int = 0 ) : Int =
        if remainingChildren.isEmpty then
            numVars
        else
            remainingChildren.head match
                case varDec : VarDec => getNumVars(remainingChildren.tail, numVars+varDec.getNumIDs(varDec.children))
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

    def getNumIDs(remainingChildren : List[ProgramElement], numIDs : Int = 0): Int =
        if remainingChildren.isEmpty then
            numIDs
        else
            remainingChildren.head match
                case IDToken(id) => getNumIDs(remainingChildren.tail, numIDs+1)
                case _ => getNumIDs(remainingChildren.tail, numIDs)

case class StatementList( override val children : List[ProgramElement] ) extends ProgramElement(children, "statements"):

    override def generateCode(state : CodeGeneratorState) : CodeGeneratorState = generateChildCode(state, children)

case class LetStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "letStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        val popCmds = children.drop(1).head match
            case IDToken(id) =>
                if state.classSymTable.map.contains(id) then
                    val sym = state.classSymTable.map(id)
                    List("pop " + sym.kind + " " + sym.index)
                else if state.subSymTable.map.contains(id) then
                    val sym = state.subSymTable.map(id)
                    List("pop " + sym.kind + " " + sym.index)
                else
                    Nil
            case _ => Nil
        state.copy(lines = childState.lines ++ popCmds)
        
case class IfStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "ifStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val exprState = children.drop(2).head.generateCode(state)
        val skipLabel = "IF-SKIP-" + state.lines.length.toString()
        val bodyState = children.drop(5).head.generateCode(state.copy(lines = exprState.lines ++ List("not", "if-goto " + skipLabel)))
        if children.length > 7 then
            val elseSkipLabel = "ELSE-SKIP-" + state.lines.length.toString()
            val elseBodyState = children.drop(9).head.generateCode(state.copy(lines = bodyState.lines ++ List("goto " + elseSkipLabel, "label " + skipLabel)))
            state.copy(lines = elseBodyState.lines ++ List("label " + elseSkipLabel))
        else
            state.copy(lines = bodyState.lines ++ List("label " + skipLabel))

case class WhileStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "whileStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val beginLabel = "WHILE-BEGIN-" + state.lines.length.toString()
        val endLabel = "WHILE-END-" + state.lines.length.toString()
        val exprState = children.drop(2).head.generateCode(state.copy(lines = state.lines ++ List("label " + beginLabel)))
        val bodyState = children.drop(5).head.generateCode(state.copy(lines = exprState.lines ++ List("not", "if-goto " + endLabel)))
        state.copy(lines = bodyState.lines ++ List("goto " + beginLabel, "label " + endLabel))

case class DoStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "doStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val ids = getIdentifiers(children)
        val pushCmds = if ids.length == 2 then
            val objName = ids.head
            val methodName = ids.drop(1).head
            if state.classSymTable.map.contains(objName) then
                val objSym = state.classSymTable.map(objName)
                List("push " + objSym.kind + " " + objSym.index.toString())
            else if state.subSymTable.map.contains(objName) then
                val objSym = state.subSymTable.map(objName)
                List("push " + objSym.kind + " " + objSym.index.toString())
            else
                Nil
        else
            List("push pointer 0")
        val doState = CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, state.lines ++ pushCmds)
        val childState = generateChildCode(doState, children)
        val numArgs = getNumArgs(children)
        val callCmds = if ids.length == 2 then
            val objName = ids.head
            val methodName = ids.drop(1).head
            if state.classSymTable.map.contains(objName) then
                val objSym = state.classSymTable.map(objName)
                List("call " + objSym.symType + "." + methodName + " " + (numArgs+1).toString())
            else if state.subSymTable.map.contains(objName) then
                val objSym = state.subSymTable.map(objName)
                List("call " + objSym.symType + "." + methodName + " " + (numArgs+1).toString())
            else
                List("call " + objName + "." + methodName + " " + numArgs.toString())
        else
            List("call " + state.className + "." + ids.head + " " + (numArgs+1).toString())
        CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, childState.lines ++ callCmds ++ List("pop temp 0"))

    def getIdentifiers(remainingChildren: List[ProgramElement], ids: List[String] = Nil): List[String] =
        if remainingChildren.isEmpty then
            return ids
        else
            val newIDs = remainingChildren.head match
                case IDToken(id) => List(id)
                case _ => Nil
            getIdentifiers(remainingChildren.tail, ids ++ newIDs)

    def getNumArgs(remainingChildren: List[ProgramElement], numArgs: Int = 0): Int =
        if remainingChildren.isEmpty then
            return numArgs
        else
            val numNewArgs = remainingChildren.head match
                case exprList : ExpressionList => exprList.getNumExprs(exprList.children)
                case _ => 0
            getNumArgs(remainingChildren.tail, numArgs + numNewArgs)

case class ReturnStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "returnStatement"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        val voidCmds = if children.length == 2 then List("push constant 0") else Nil
        CodeGeneratorState(state.className, state.classSymTable, state.subSymTable, childState.lines ++ voidCmds ++ List("return"))

case class Expression( override val children : List[ProgramElement], lastSym : SymbolToken ) extends ProgramElement(children, "expression"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        val childState = generateChildCode(state, children)
        children.head match
            case SymbolToken('-') => state.copy(lines = childState.lines ++ List("neg"))
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
                case SymbolToken('*') => List("call Math.multiply 2")
                case SymbolToken('/') => List("call Math.divide 2")
                case _ => Nil
            if opCmds.isEmpty then
                generateOperatorCode(state, remainingChildren.tail)
            else
                state.copy(lines = state.lines ++ opCmds)
            
case class ExpressionTerm( override val children : List[ProgramElement] ) extends ProgramElement(children, "term"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState =
        if children.length == 1 then
            children.head match
                case IDToken(id) =>
                    if state.subSymTable.map.contains(id) then
                        val termSym = state.subSymTable.map(id)
                        val pushCmds = List("push " + termSym.kind + " " + termSym.index.toString())
                        state.copy(lines = state.lines ++ pushCmds)
                    else if state.classSymTable.map.contains(id) then
                        val termSym = state.classSymTable.map(id)
                        val pushCmds = List("push " + termSym.kind + " " + termSym.index.toString())
                        state.copy(lines = state.lines ++ pushCmds)
                    else
                        val childState = generateChildCode(state, children)
                        val callCmds = List("call " + id)
                        state.copy(lines = childState.lines ++ callCmds)
                case IntegerToken(int) =>
                    val pushCmds = List("push constant " + int.toString())
                    state.copy(lines = state.lines ++ pushCmds)
                case KeywordToken("true") =>
                    val pushCmds = List("push constant 1", "neg")
                    state.copy(lines = state.lines ++ pushCmds)
                case KeywordToken("false") | KeywordToken("null")=>
                    val pushCmds = List("push constant 0")
                    state.copy(lines = state.lines ++ pushCmds)
                case _ => state
        else if children.length == 2 then
            children.head match
                case SymbolToken('-') =>
                    val childState = generateChildCode(state, children)
                    state.copy(lines = childState.lines ++ List("neg"))
                case SymbolToken('~') =>
                    val childState = generateChildCode(state, children)
                    state.copy(lines = childState.lines ++ List("not"))
                case _ => generateChildCode(state, children)
        else if children.length == 4 then
            children.head match
                case IDToken(id) =>
                    val childState = generateChildCode(state, children)
                    val numArgs = children.drop(2).head match
                        case exprList: ExpressionList => exprList.getNumExprs(exprList.children)
                        case _ => 0
                    state.copy(lines = childState.lines ++ List("call " + id + " " + numArgs.toString()))
                case _ => state
        else if children.length == 6 then
            children.head match
                case IDToken(classID) =>
                    children.drop(2).head match
                        case IDToken(methodID) =>
                            val childState = generateChildCode(state, children)
                            val numArgs = children.drop(4).head match
                                case exprList: ExpressionList => exprList.getNumExprs(exprList.children)
                                case _ => 0
                            val callCmds = List("call " + classID + "." + methodID + " " + numArgs.toString())
                            state.copy(lines = childState.lines ++ callCmds)
                        case _ => state
                case _ => state
        else
            generateChildCode(state, children)
 
case class ExpressionList( override val children : List[ProgramElement] ) extends ProgramElement(children, "expressionList"):

    override def generateCode(state: CodeGeneratorState): CodeGeneratorState = generateChildCode(state, children)

    def getNumExprs(remainingChildren: List[ProgramElement], numExprs: Int = 0): Int =
        if remainingChildren.isEmpty then
            numExprs
        else
            val numNewExprs = remainingChildren.head match
                case expr: Expression => 1
                case _ => 0
            getNumExprs(remainingChildren.tail, numExprs + numNewExprs)

