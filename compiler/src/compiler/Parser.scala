
package compiler

import java.io.File
import java.io.PrintWriter
import scala.io.Source

class ProgramElement( val children : List[ProgramElement], val xmlTagName : String ):
    // def getXML : String
    def writeXML( writer : PrintWriter, indent : String = "" ) : Unit =
        writer.println(indent + "<" + xmlTagName + ">")
        for child <- children do
            child.writeXML(writer, indent + "  ")
        writer.println(indent + "</" + xmlTagName + ">")

case class ClassElement( override val children : List[ProgramElement] ) extends ProgramElement(children, "class")
case class ClassVarDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "classVarDec")
case class SubroutineDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineDec")
case class ParameterList( override val children : List[ProgramElement] ) extends ProgramElement(children, "parameterList")
case class SubroutineBody( override val children : List[ProgramElement] ) extends ProgramElement(children, "subroutineBody")
case class VarDec( override val children : List[ProgramElement] ) extends ProgramElement(children, "varDec")
case class StatementList( override val children : List[ProgramElement] ) extends ProgramElement(children, "statements")
case class LetStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "letStatement")
case class IfStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "ifStatement")
case class WhileStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "whileStatement")
case class DoStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "doStatement")
case class ReturnStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "returnStatement")
case class Expression( override val children : List[ProgramElement] ) extends ProgramElement(children, "expression")
case class ExpressionTerm( override val children : List[ProgramElement] ) extends ProgramElement(children, "term")
case class ExpressionList( override val children : List[ProgramElement] ) extends ProgramElement(children, "expressionList")

class Parser (val file : File):

    def parse : Unit =
        val srcFileIter = if file.isFile() then Iterator[File](file) else file.listFiles().iterator
        for srcFile <- srcFileIter if srcFile.getName().endsWith(".jack") do
            val compiler = Compiler(Source.fromFile(srcFile))
            val xmlFilePath = srcFile.getPath().substring(0, srcFile.getPath().length()-5) + "_mine.xml"
            val xmlWriter = PrintWriter(xmlFilePath)
            while compiler.hasNextToken() do
                compiler.nextToken() match
                    case Some(kw : KeywordToken) =>
                        parseClass(compiler, List(kw)) match
                            case Some(newClass : ClassElement) => newClass.writeXML(xmlWriter)
                            case _ =>
                    case _ =>
            xmlWriter.close()

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
                                parseExpression(compiler, List(exprToken)) match
                                    case Some(expr : Expression) => List(sym, expr)
                                    case _ => Nil
                            case _ => Nil
                    case _ => List(sym)
            case child : ProgramElement => List(child)
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
                                parseExpression(compiler, List(exprToken)) match
                                    case Some(expr : Expression) => List(sym, expr)
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
                                parseExpression(compiler, List(exprToken)) match
                                    case Some(expr : Expression) => List(sym, expr)
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
            parseExpression(compiler, List(nextToken.head)) match
                case Some(expr : Expression) => List(expr)
                case _ => Nil
        parseReturnStatement(compiler, children ::: nextChild)

    def parseExpression( compiler : Compiler, children : List[ProgramElement] ) : Option[Expression] =
        Some(Expression(List(ExpressionTerm(children))))

    def parseExpressionList( compiler : Compiler, children : List[ProgramElement] ) : Option[ExpressionList] =
        val nextToken = compiler.nextToken() match
            case Some(token : Token) => List(token)
            case _ => Nil
        if nextToken.isEmpty then
            return None
        else if nextToken.head == SymbolToken(')') then
            return Some(ExpressionList(children))
        val nextChild = nextToken.head match
            case id : IDToken =>
                parseExpression(compiler, List(id)) match
                    case Some(expr : Expression) => List(expr)
                    case _ => Nil
            case kw : KeywordToken =>
                kw.kw match
                    case "true" | "false" | "null" | "this" =>
                        parseExpression(compiler, List(kw)) match
                            case Some(expr : Expression) => List(expr)
                            case _ => Nil
                    case _ => List(kw)
            case child : ProgramElement => List(child)
        parseExpressionList(compiler, children ::: nextChild)

object Parser {

    def main(args : Array[String]) : Unit = {
        val parser = Parser(File(args(0)))
        parser.parse
    }
}
