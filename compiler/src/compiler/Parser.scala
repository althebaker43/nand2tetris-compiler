
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
case class doStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "doStatement")
case class returnStatement( override val children : List[ProgramElement] ) extends ProgramElement(children, "returnStatement")

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
                    case _ => List(kw)
            case child : ProgramElement => List(child)
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
        

object Parser {

    def main(args : Array[String]) : Unit = {
        val parser = Parser(File(args(0)))
        parser.parse
    }
}
