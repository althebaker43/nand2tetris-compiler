
package compiler

import java.io.File
import java.io.PrintWriter
import scala.io.Source
import scala.compiletime.ops.double
import java.awt.RenderingHints.Key
import java.lang.reflect.Parameter

class ProgramElement( children : List[ProgramElement] ) {
    // def getXML : String
    def writeXML( writer : PrintWriter, indent : String = "" ) : Unit =
        for child <- children do
            child.writeXML(writer, indent + "  ")
}

case class ClassElement( children : List[ProgramElement] ) extends ProgramElement(children) {
    // def getXML : String = "<class><identifier>" + name + "</identifier></class>"<identifier>" + name + "</identifier></class>"
    override def writeXML(writer: PrintWriter, indent : String = ""): Unit =
        writer.println(indent + "<class>")
        super.writeXML(writer, indent)
        writer.println(indent + "</class>")
}

case class ClassVarDec( children : List[ProgramElement] ) extends ProgramElement(children):
    override def writeXML(writer: PrintWriter, indent: String): Unit =
        writer.println(indent + "<classVarDec>")
        super.writeXML(writer, indent)
        writer.println(indent + "</classVarDec>")

case class SubroutineDec( children : List[ProgramElement] ) extends ProgramElement(children):
    override def writeXML(writer: PrintWriter, indent: String): Unit =
        writer.println(indent + "<subroutineDec>")
        super.writeXML(writer, indent)
        writer.println(indent + "</subroutineDec>")

case class ParameterList( children : List[ProgramElement] ) extends ProgramElement(children):
    override def writeXML(writer: PrintWriter, indent: String): Unit =
        writer.println(indent + "<parameterList>")
        super.writeXML(writer, indent)
        writer.println(indent + "</parameterList")

class Parser (val file : File) {

    def parse : Unit = {
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
    }

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
            return Some(SubroutineDec(children))
        val nextChild = nextToken.head match
            case openParenSymbol : SymbolToken =>
                openParenSymbol.sym match
                    case '(' =>
                        parseParameterList(compiler, Nil) match
                            case Some(list : ParameterList) => List(openParenSymbol, list)
                            case _ => Nil
                    case _ => List(openParenSymbol)
            case child : ProgramElement => List(child)
        parseSubroutineDec(compiler, children ::: nextChild)

    def parseParameterList( compiler : Compiler, children : List[ProgramElement] ) : Option[ParameterList] = Some(ParameterList(children))
}

object Parser {

    def main(args : Array[String]) : Unit = {
        val parser = Parser(File(args(0)))
        parser.parse
    }
}
