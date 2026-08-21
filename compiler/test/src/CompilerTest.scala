package compiler

import scala.io.Source
import java.io.File
import java.nio.file.{Path, Files, SimpleFileVisitor, FileVisitResult, StandardCopyOption, FileAlreadyExistsException}
import java.nio.file.attribute.BasicFileAttributes
import java.util.function.Consumer
import scala.compiletime.ops.double
import java.io.PrintWriter

class ResourceCopier( val destDirPath : Path ) extends Consumer[Path]:
    def accept( srcFilePath : Path ) : Unit =
        val destFilePath = Path.of(destDirPath.toString(), srcFilePath.getFileName().toString())
        Files.copy(srcFilePath, destFilePath, StandardCopyOption.REPLACE_EXISTING)

class CompilerSuite extends munit.FunSuite {

    def getTestDir( name : String ) : File =
        val srcPath = Path.of(getClass.getResource("/" + name + "/").toURI())
        val destPath = Path.of("test_run_dir", name)
        try
            Files.createDirectories(destPath)
        catch
            case ex : FileAlreadyExistsException =>
        Files.list(srcPath).forEach(ResourceCopier(destPath))
        File(destPath.toUri())

    val listing = """
// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/10/ExpressionLessSquare/Main.jack

/** Expressionless version of projects/10/Square/Main.jack. */

class Main {
    static boolean test;    // Added for testing -- there is no static keyword
                            // in the Square files.

    function void main() {
        var SquareGame game;
        let game = game;
        do game.run();
        do game.dispose();
        return;
    }

    function void more() {  // Added to test Jack syntax that is not used in
        var boolean b;      // the Square files.
        if (b) {
        }
        else {              // There is no else keyword in the Square files.
        }
        return;
    }
}
    """

    test("charIter") {

        val charIter = Source.fromURL(getClass.getResource("/Square/Main.jack"))
        assertEquals('/', charIter.next())
        assertEquals('/', charIter.ch)
        assertEquals('/', charIter.next())
        assertEquals('/', charIter.ch)
        assertEquals(' ', charIter.next())
        assertEquals(' ', charIter.ch)
        assertEquals('T', charIter.next())
        assertEquals('T', charIter.ch)
        assertEquals('h', charIter.next())
    }

    test("tokenIter") {

        val compiler = new Compiler(Source.fromURL(getClass.getResource("/Square/Main.jack")))

        val classToken = compiler.nextToken()
        assert(!classToken.isEmpty)
        assertEquals(classToken.get, KeywordToken("class"))

        val mainToken = compiler.nextToken()
        assert(!mainToken.isEmpty)
        assertEquals(mainToken.get, IDToken("Main"))

        val classOpenBraceToken = compiler.nextToken()
        assert(!classOpenBraceToken.isEmpty)
        assertEquals(classOpenBraceToken.get, SymbolToken('{'))
    }

    test("sevenCodeGen") {
        val expectedCmds = List(
            "function Main.main 0",
            "push constant 1",
            "push constant 2",
            "push constant 3",
            "call Math.multiply 2",
            "add",
            "call Output.printInt 1",
            "pop temp 0",
            "push constant 0",
            "return",
        )
        val testDir = getTestDir("Seven")
        val parser = Parser(testDir)
        for classElementOpt <- parser.parse do
            classElementOpt match
                case Some(classElement : ClassElement) =>
                    val classSymTable = SymbolTable(Map[String, CodeSymbol]())
                    val subSymTable = SymbolTable(Map[String, CodeSymbol]())
                    for child <- classElement.children do
                        child match
                            case IDToken(id) =>
                                val codeLines = classElement.generateCode(CodeGeneratorState(id, classSymTable, subSymTable, List[String]())).lines
                                val vmFile = File(testDir.getPath + "/" + id + ".vm")
                                val vmWriter = PrintWriter(vmFile)
                                codeLines.foreach(vmWriter.println(_))
                                vmWriter.close()
                                assert(codeLines.length > 0)
                                assertEquals(codeLines, expectedCmds, "Unexpected commands")
                            case _ =>
                case _ => fail("Failed to parse class")
    }

    test("averageCodeGen") {
        val parser = Parser(getTestDir("Average"))
        for classElementOpt <- parser.parse do
            classElementOpt match
                case Some(classElement : ClassElement) =>
                    val classSymTable = SymbolTable(Map[String, CodeSymbol]())
                    val subSymTable = SymbolTable(Map[String, CodeSymbol]())
                    for child <- classElement.children do
                        child match
                            case IDToken(id) =>
                                val codeLines = classElement.generateCode(CodeGeneratorState(id, classSymTable, subSymTable, List[String]())).lines
                                // assert(codeLines.length > 0)
                            case _ =>
                case _ => fail("Failed to parse class")
    }

    test("complexArraysCodeGen") {
        val parser = Parser(getTestDir("ComplexArrays"))
        parser.parse
    }

    test("convertToBinCodeGen") {
        val parser = Parser(getTestDir("ConvertToBin"))
        parser.parse
    }

    test("pongCodeGen") {
        val parser = Parser(getTestDir("Pong"))
        parser.parse
    }

    test("squareCodeGen") {
        val parser = Parser(getTestDir("Square"))
        parser.parse
    }
}