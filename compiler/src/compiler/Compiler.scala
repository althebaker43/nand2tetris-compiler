package compiler

import scala.io.Source
import java.io.File

abstract class Token {
  def getXML : String
}

case class KeywordToken(kw : String) extends Token {
  override def getXML : String = "<keyword> " + kw + " </keyword>"
}

case class SymbolToken(sym : Char) extends Token {
  override def getXML : String =
    sym match
      case '<' => "<symbol> &lt; </symbol>"
      case '>' => "<symbol> &gt; </symbol>"
      case '&' => "<symbol> &amp; </symbol>"
      case _ => "<symbol> " + sym + " </symbol>"
}

case class IntegerToken(int : scala.Int) extends Token {
  override def getXML: String = "<integerConstant> " + int.toString() + " </integerConstant>"
}

case class StringToken(str : String) extends Token {
  override def getXML: String = "<stringConstant> " + str + " </stringConstant>"
}

case class IDToken(id : String) extends Token {
  override def getXML: String = "<identifier> " + id + " </identifier>"
}

class Compiler (val src : Source) {

  private var curChar : Char = '\u0000'
  private var isSrcDone : Boolean = false

  // def hasNextToken() : Boolean = src.hasNext || (curChar != '\u0000')

  def atToken() : Boolean = {
    if curChar.isWhitespace then
      false
    else if curChar == '\u0000' then
      false
    else if isSrcDone then
      true
    else {
      if curChar == '/' && src.ch == '/' then
        false
      else if curChar == '/' && src.ch == '*' then
        false
      else
        true
    }
  }

  def atEof() : Boolean = curChar == '\u0000' && !src.hasNext

  def nextChar() : Char = {

    if isSrcDone then
      curChar = '\u0000'
    else
      curChar = src.ch

    if src.hasNext then
      src.next()
    else
      isSrcDone = true

    curChar
  }

  def eatWhitespace() : Unit = {
    while curChar.isWhitespace && !atEof() do nextChar()
  }

  def eatComment() : Unit = {
    while curChar != '\n' && !atEof() do nextChar()
  }

  def eatBlockComment() : Unit = {
    while !(curChar == '*' && src.ch == '/') && !atEof() do nextChar()
    nextChar()
    nextChar()
  }

  def hasNextToken() : Boolean = {

    while !atToken() && !atEof() do {

      // Initialization
      if curChar == '\u0000' then {
        if src.hasNext then {
          curChar = src.next()
          if src.hasNext then
            src.next()
          else
            return false
        } else
          return false
      }

      if curChar.isWhitespace then
        eatWhitespace()
      else if curChar == '/' && src.ch == '/' then
        eatComment()
      else if curChar == '/' && src.ch == '*' then
        eatBlockComment()
    }

    !atEof()
  }

  def nextKeywordOrIdentifier(curStr : String = "") : Token = {
    if !curChar.isLetterOrDigit && curChar != '_' then {
      curStr match
        case "class" | "constructor" | "function" | "method" => KeywordToken(curStr)
        case "field" | "static" | "var" | "int" => KeywordToken(curStr)
        case "char" | "boolean" | "void" | "true" | "false" => KeywordToken(curStr)
        case "null" | "this" | "let" | "do" | "if" | "else" => KeywordToken(curStr)
        case "while" | "return" => KeywordToken(curStr)
        case _ => IDToken(curStr)
    } else {
      val prevChar = curChar
      nextChar()
      nextKeywordOrIdentifier(curStr + prevChar)
    }
  }

  val symbols = Set('{', '}', '(', ')', '[', ']', '.', ',', ';', '+', '-', '*', '/', '&', '|', '<', '>', '=', '~')

  def nextInteger(curStr : String = "") : Token =
    if !curChar.isDigit then
      IntegerToken(curStr.toInt)
    else
      val prevChar = curChar
      nextChar()
      nextInteger(curStr + prevChar)

  def nextString(curStr : String = "") : Token =
    if curChar == '"' then
      nextChar()
      StringToken(curStr)
    else
      val prevChar = curChar
      nextChar()
      nextString(curStr + prevChar)

  // TODO: rewrite to be higher-order, recursive
  def nextToken() : Option[Token] = {
    if !hasNextToken() then return None
    if symbols.contains(curChar) then
      val symbolChar = curChar
      nextChar()
      Some(SymbolToken(symbolChar))
    else if curChar.isLetter || curChar == '_' then
      Some(nextKeywordOrIdentifier())
    else if curChar.isDigit then
      Some(nextInteger())
    else if curChar == '"' then
      nextChar()
      Some(nextString())
    else
      None
  }

  def printXML : Unit = {
    println("<tokens>")
    while hasNextToken() do
      nextToken() match
        case Some[Token](token) => println(token.getXML)
        case None =>
    println("</tokens>")
  }
}

object Compiler {
  def main(args : Array[String]) : Unit = {
    val srcFile = File(args(0))
    val compiler = new Compiler(Source.fromFile(srcFile))
    compiler.printXML
  }
}
