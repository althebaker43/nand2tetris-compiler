
package compiler

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import javax.net.ssl.TrustManager

object Parser {

    def main(args : Array[String]) : Unit = {
        val inputFile = File(args(0))
        val parser = Parser(inputFile)
        val outputPath = if inputFile.isFile() then inputFile.getParent() else inputFile.getPath()
        parser.generateCode(outputPath)
    }

    def writeCodeLines(lines: List[String], outputPath: String): Unit = {
        val vmFile = File(outputPath)
        val vmWriter = PrintWriter(vmFile)
        lines.foreach(Parser.writeCodeLine(_, vmWriter))
        vmWriter.close()
    }

    def writeCodeLine(line: String, writer: PrintWriter): Unit = {
        if line.startsWith("function") then
            writer.println("\n" + line)
        else
            writer.println("  " + line)
    }
}

class Parser (val file : File):

    def parse : Iterator[Option[ClassElement]] =
        val srcFileIter = if file.isFile() then Iterator[File](file) else file.listFiles().iterator
        for srcFile <- srcFileIter if srcFile.getName().endsWith(".jack") yield
            println("Parsing file " + srcFile.getName())
            val compiler = Compiler(Source.fromFile(srcFile))
            val xmlFilePath = srcFile.getPath().substring(0, srcFile.getPath().length()-5) + "_syntax.xml"
            val xmlWriter = PrintWriter(xmlFilePath)
            val classElement = if compiler.hasNextToken() then
                compiler.nextToken() match
                    case Some(kw : KeywordToken) =>
                        parseClass(compiler, List(kw)) match
                            case Some(newClass : ClassElement) =>
                                newClass.writeXML(xmlWriter)
                                Some(newClass)
                            case _ => None
                    case _ => None
                else
                    None
            xmlWriter.close()
            classElement

    def generateCode( outputPath : String ) : Unit =
        for classElementOpt <- parse do
            classElementOpt match
                case Some(classElement : ClassElement) =>
                    val classSymTable = SymbolTable(Map[String, CodeSymbol]())
                    val subSymTable = SymbolTable(Map[String, CodeSymbol]())
                    for child <- classElement.children do
                        child match
                            case IDToken(id) =>
                                val codeLines = classElement.generateCode(CodeGeneratorState(id, classSymTable, subSymTable, List[String]())).lines
                                Parser.writeCodeLines(codeLines, outputPath + "/" + id + ".vm")
                            case _ =>
                case _ =>

    def parseClass( compiler : Compiler, children : List[ProgramElement] ) : Option[ClassElement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('}') then
            return Some(ClassElement(children ::: nextToken))
        val nextChild = nextToken.head match
            case memberKW : KeywordToken =>
                memberKW.kw match
                    case "static" | "field" =>
                        parseClassVarDec(compiler, List(memberKW)) match
                            case Some(nextElement : ProgramElement) => List(nextElement)
                            case _ => Nil
                    case "constructor" | "function" | "method" =>
                        parseSubroutineDec(compiler, List(memberKW)) match
                            case Some(nextElement : ProgramElement) => List(nextElement)
                            case _ => Nil
                    case _ => List(memberKW)
            case child : ProgramElement => List(child)
        parseClass(compiler, children ::: nextChild)

    def parseClassVarDec( compiler : Compiler, children : List[ProgramElement] ) : Option[ClassVarDec] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(';') then
            return Some(ClassVarDec(children ::: nextToken))
        parseClassVarDec(compiler, children ::: nextToken)

    def parseSubroutineDec( compiler : Compiler, children : List[ProgramElement] ) : Option[SubroutineDec] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('{') then
            return parseSubroutineBody(compiler, nextToken) match
                case Some(body : SubroutineBody) => Some(SubroutineDec(children ::: List(body)))
                case _ => None
        val nextChild = nextToken.head match
            case openParenSymbol : SymbolToken =>
                openParenSymbol.sym match
                    case '(' =>
                        parseParameterList(compiler, Nil) match
                            case Some(list : ParameterList) => List(openParenSymbol, list, SymbolToken(')'))
                            case _ => Nil
                    case _ => List(openParenSymbol)
            case child : ProgramElement => List(child)
        parseSubroutineDec(compiler, children ::: nextChild)

    def parseParameterList( compiler : Compiler, children : List[ProgramElement] ) : Option[ParameterList] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(')') then
            return Some(ParameterList(children))
        parseParameterList(compiler, children ::: nextToken)

    def parseSubroutineBody( compiler : Compiler, children : List[ProgramElement] ) : Option[SubroutineBody] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('}') then
            return Some(SubroutineBody(children ::: nextToken))
        val nextChild = nextToken.head match
            case kw : KeywordToken =>
                kw.kw match
                    case "var" =>
                        parseVarDec(compiler, List(kw)) match
                            case Some(varDec : VarDec) => List(varDec)
                            case _ => Nil
                    case _ =>
                        parseStatementList(compiler, Nil, Some(kw)) match
                            case Some(statements : StatementList) => List(statements, SymbolToken('}'))
                            case _ => Nil
            case child : ProgramElement => List(child)
        if nextChild.last == SymbolToken('}') then
            return Some(SubroutineBody(children ::: nextChild))
        parseSubroutineBody(compiler, children ::: nextChild)

    def parseVarDec( compiler : Compiler, children : List[ProgramElement] ) : Option[VarDec] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(';') then
            return Some(VarDec(children ::: nextToken))
        parseVarDec(compiler, children ::: nextToken)

    def parseStatementList( compiler : Compiler, children : List[ProgramElement], firstToken : Option[Token] = None ) : Option[StatementList] =
        val nextToken = firstToken match
            case Some(token : Token) => List(token)
            case _ => compiler.nextToken() match
                case Some(token : Token) => List(token)
                case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('}') then
            return Some(StatementList(children))
        val prevChildren = nextToken.head match
            case kw : KeywordToken =>
                kw.kw match
                    case "else" => children.dropRight(1)
                    case _ => children
            case _ => children
        val nextChild = nextToken.head match
            case kw : KeywordToken =>
                kw.kw match
                    case "let" =>
                        parseLetStatement(compiler, List(kw)) match
                            case Some(letStatement : LetStatement) => List(letStatement)
                            case _ => Nil
                    case "do" =>
                        parseDoStatement(compiler, List(kw)) match
                            case Some(doStatement : DoStatement) => List(doStatement)
                            case _ => Nil
                    case "if" =>
                        parseIfStatement(compiler, List(kw)) match
                            case Some(ifStatement : IfStatement) => List(ifStatement)
                            case _ => Nil
                    case "else" =>
                        parseIfStatement(compiler, children.last.children ::: List(kw)) match
                            case Some(ifStatement : IfStatement) => List(ifStatement)
                            case _ => Nil
                    case "while" =>
                        parseWhileStatement(compiler, List(kw)) match
                            case Some(whileStatement : WhileStatement) => List(whileStatement)
                            case _ => Nil
                    case "return" =>
                        parseReturnStatement(compiler, List(kw)) match
                            case Some(returnStatement : ReturnStatement) => List(returnStatement)
                            case _ => Nil
                    case _ => List(kw)
            case child : ProgramElement => List(child)
        parseStatementList(compiler, prevChildren ::: nextChild)

    def parseLetStatement( compiler : Compiler, children : List[ProgramElement] ) : Option[LetStatement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(';') then
            return Some(LetStatement(children ::: nextToken))
        val nextChild = nextToken.head match
            case sym : SymbolToken =>
                sym.sym match
                    case '[' | '=' =>
                        compiler.nextToken() match
                            case Some(exprToken : Token) =>
                                parseExpression(compiler, Nil, Some(exprToken)) match
                                    case Some(expr : Expression) =>
                                        sym.sym match
                                            case '[' => List(sym, expr, SymbolToken(']'))
                                            case '=' => List(sym, expr, SymbolToken(';'))
                                            case _ => List(sym, expr)
                                    case _ => Nil
                            case _ => Nil
                    case _ => List(sym)
            case child : ProgramElement => List(child)
        if !nextChild.isEmpty && (nextChild.last == SymbolToken(';')) then
            return Some(LetStatement(children ::: nextChild))
        parseLetStatement(compiler, children ::: nextChild)

    def parseDoStatement( compiler : Compiler, children : List[ProgramElement] ) : Option[DoStatement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(';') then
            return Some(DoStatement(children ::: nextToken))
        val nextChild = nextToken.head match
            case sym : SymbolToken =>
                sym.sym match
                    case '(' =>
                        parseExpressionList(compiler, Nil) match
                            case Some(exprList : ExpressionList) => List(sym, exprList, SymbolToken(')'))
                            case _ => Nil
                    case _ => List(sym)
            case child : ProgramElement => List(child)
        parseDoStatement(compiler, children ::: nextChild)

    def parseIfStatement( compiler : Compiler, children : List[ProgramElement] ) : Option[IfStatement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('}') then
            return Some(IfStatement(children ::: nextToken))
        val nextChild = nextToken.head match
            case sym : SymbolToken =>
                sym.sym match
                    case '(' =>
                        compiler.nextToken() match
                            case Some(exprToken : Token) =>
                                parseExpression(compiler, Nil, Some(exprToken)) match
                                    case Some(expr : Expression) => List(sym, expr, SymbolToken(')'))
                                    case _ => Nil
                            case _ => Nil
                    case '{' =>
                        parseStatementList(compiler, Nil) match
                            case Some(statements : StatementList) => List(sym, statements, SymbolToken('}'))
                            case _ => Nil
                    case _ => List(sym)
            case child : ProgramElement => List(child)
        if nextChild.last == SymbolToken('}') then
            return Some(IfStatement(children ::: nextChild))
        parseIfStatement(compiler, children ::: nextChild)

    def parseWhileStatement( compiler : Compiler, children : List[ProgramElement] ) : Option[WhileStatement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken('}') then
            return Some(WhileStatement(children ::: nextToken))
        val nextChild = nextToken.head match
            case sym : SymbolToken =>
                sym.sym match
                    case '(' =>
                        compiler.nextToken() match
                            case Some(exprToken : Token) =>
                                parseExpression(compiler, Nil, Some(exprToken)) match
                                    case Some(expr : Expression) => List(sym, expr, SymbolToken(')'))
                                    case _ => Nil
                            case _ => Nil
                    case '{' =>
                        parseStatementList(compiler, Nil) match
                            case Some(statements : StatementList) => List(sym, statements, SymbolToken('}'))
                            case _ => Nil
                    case _ => List(sym)
            case child : ProgramElement => List(child)
        if nextChild.last == SymbolToken('}') then
            return Some(WhileStatement(children ::: nextChild))
        parseWhileStatement(compiler, children ::: nextChild)

    def parseReturnStatement( compiler : Compiler, children : List[ProgramElement] ) : Option[ReturnStatement] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(';') then
            return Some(ReturnStatement(children ::: nextToken))
        val nextChild =
            parseExpression(compiler, Nil, Some(nextToken.head)) match
                case Some(expr : Expression) => List(expr, SymbolToken(';'))
                case _ => Nil
        Some(ReturnStatement(children ::: nextChild))

    def parseExpression( compiler : Compiler, children : List[ProgramElement], firstToken : Option[Token] = None ) : Option[Expression] =
        val nextToken = 
            firstToken match
                case Some(token : Token) => List(token)
                case _ => compiler.nextToken() match
                    case Some(token : Token) => List(token)
                    case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(')') then
            return Some(Expression(children, SymbolToken(')')))
        else if nextToken.head == SymbolToken(']') then
            return Some(Expression(children, SymbolToken(']')))
        else if nextToken.head == SymbolToken(';') then
            return Some(Expression(children, SymbolToken(';')))
        else if nextToken.head == SymbolToken(',') then
            return Some(Expression(children, SymbolToken(',')))
        val nextChild = firstToken match
            case Some(sym : SymbolToken) =>
                sym.sym match
                    case '-' | '~' | '(' =>
                        parseExpressionTerm(compiler, Nil, firstToken, None) match
                            case Some(term : ExpressionTerm) => List(term)
                            case _ => Nil
                    case _ => List(sym)
            case Some(token : Token) =>
                compiler.nextToken() match
                    case Some(symToken : SymbolToken) =>
                        symToken.sym match
                            case '.' | '(' | '[' =>
                                parseExpressionTerm(compiler, Nil, firstToken, Some(symToken)) match
                                    case Some(term : ExpressionTerm) => List(term)
                                    case _ => Nil
                            case _ =>
                                parseExpressionTerm(compiler, Nil, firstToken, None) match
                                    case Some(term : ExpressionTerm) => List(term, symToken)
                                    case _ => Nil
                    case _ => Nil
            case _ =>
                nextToken.head match
                    case sym : SymbolToken =>
                        sym.sym match
                            case '(' =>
                                parseExpressionTerm(compiler, Nil, Some(sym), None) match
                                    case Some(term : ExpressionTerm) => List(term)
                                    case _ => Nil
                            case '[' =>
                                parseExpression(compiler, Nil) match
                                    case Some(expr : Expression) => List(sym, expr, SymbolToken(']'))
                                    case _ => Nil
                            // case '-' | '~' =>
                            //     parseExpressionTerm(compiler, Nil, Some(sym), None) match
                            //         case Some(term : ExpressionTerm) => List(term)
                            //         case _ => Nil
                            case _ => List(sym)
                    case _ =>
                        compiler.nextToken() match
                            case Some(symToken : SymbolToken) =>
                                symToken.sym match
                                    case ')' | ']' | ';' | ',' =>
                                        parseExpressionTerm(compiler, Nil, Some(nextToken.head), None) match
                                            case Some(term : ExpressionTerm) => List(term, symToken)
                                            case _ => Nil
                                    case '(' | '[' | '.' =>
                                        parseExpressionTerm(compiler, Nil, Some(nextToken.head), Some(symToken)) match
                                            case Some(term : ExpressionTerm) => List(term)
                                            case _ => Nil
                                    case _ =>
                                        parseExpressionTerm(compiler, Nil, Some(nextToken.head), None) match
                                            case Some(term : ExpressionTerm) => List(term, symToken)
                                            case _ => Nil
                            case _ => Nil
        if !nextChild.isEmpty then
            if nextChild.last == SymbolToken(')') then
                return Some(Expression(children ::: nextChild.dropRight(1), SymbolToken(')')))
            else if nextChild.last == SymbolToken(']') then
                return Some(Expression(children ::: nextChild.dropRight(1), SymbolToken(']')))
            else if nextChild.last == SymbolToken(';') then
                return Some(Expression(children ::: nextChild.dropRight(1), SymbolToken(';')))
            else if nextChild.last == SymbolToken(',') then
                return Some(Expression(children ::: nextChild.dropRight(1), SymbolToken(',')))
        parseExpression(compiler, children ::: nextChild)

    def parseExpressionTerm( compiler : Compiler, children : List[ProgramElement], firstToken : Option[Token], secondToken : Option[Token] ) : Option[ExpressionTerm] =
        val nextChild = (firstToken, secondToken) match
            case (Some(intToken : IntegerToken), _) => /* println("Parsing integer term " + intToken.int.toString); */ List(intToken)
            case (Some(strToken : StringToken), _) => /* println("Parsing string term " + strToken.str); */ List(strToken)
            case (Some(kwToken : KeywordToken), _) => /* println("Parsing keyword term " + kwToken.kw); */ List(kwToken)
            case (Some(symToken : SymbolToken), _) =>
                // println("Parsing symbol term " + symToken.sym)
                symToken.sym match
                    case '(' =>
                        secondToken match
                            case Some(token : Token) =>
                                parseExpression(compiler, Nil, secondToken) match
                                    case Some(expr : Expression) => List(symToken, expr, SymbolToken(')'))
                                    case _ => Nil
                            case _ =>
                                parseExpression(compiler, Nil, compiler.nextToken()) match
                                    case Some(expr : Expression) => List(symToken, expr, SymbolToken(')'))
                                    case _ => Nil
                    case _ =>
                        if !secondToken.isEmpty then
                            parseExpressionTerm(compiler, Nil, secondToken, compiler.nextToken()) match
                                case Some(term : ExpressionTerm) => List(symToken, term)
                                case _ => Nil
                        else
                            parseExpressionTerm(compiler, Nil, compiler.nextToken(), None) match
                                case Some(term : ExpressionTerm) => List(symToken, term)
                                case _ => Nil
            case (Some(idToken : IDToken), Some(symToken : SymbolToken)) =>
                // println("Parsing ID-symbol term pair " + idToken.id + ", " + symToken.sym)
                symToken.sym match
                    case '(' =>
                        parseExpressionList(compiler, Nil) match
                            case Some(exprs : ExpressionList) => List(idToken, symToken, exprs, SymbolToken(')'))
                            case _ => Nil
                    case '[' =>
                        parseExpression(compiler, Nil) match
                            case Some(expr : Expression) => List(idToken, symToken, expr, SymbolToken(']'))
                            case _ => Nil
                    case '.' =>
                        compiler.nextToken() match
                            case Some(nextIDToken : IDToken) =>
                                compiler.nextToken() match
                                    case Some(nextSymToken : SymbolToken) =>
                                        nextSymToken.sym match
                                            case '(' =>
                                                parseExpressionList(compiler, Nil) match
                                                    case Some(exprs : ExpressionList) => List(idToken, symToken, nextIDToken, nextSymToken, exprs, SymbolToken(')'))
                                                    case _ => Nil
                                            case _ => Nil
                                    case _ => Nil
                            case _ => Nil
                    case _ => List(idToken)
            case (Some(idToken : IDToken), _) => /* println("Parsing ID term " + idToken.id); */ List(idToken) 
            case _ => Nil
        Some(ExpressionTerm(nextChild))

    def parseExpressionList( compiler : Compiler, children : List[ProgramElement] ) : Option[ExpressionList] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(')') then
            return Some(ExpressionList(children))
        val nextChild = nextToken.head match
            case sym : SymbolToken =>
                sym.sym match
                    case ',' => Nil
                    case _ =>
                        parseExpression(compiler, Nil, Some(sym)) match
                            case Some(expr : Expression) => List(expr)
                            case _ => Nil
            case child : ProgramElement =>
                parseExpression(compiler, Nil, Some(child)) match
                    case Some(expr : Expression) => List(expr)
                    case _ => Nil
        if !nextChild.isEmpty then
            if nextChild.last.lastSym == SymbolToken(')') then
                return Some(ExpressionList(children ::: nextChild))
        parseExpressionList(compiler, children ::: nextChild ::: List(SymbolToken(',')))
