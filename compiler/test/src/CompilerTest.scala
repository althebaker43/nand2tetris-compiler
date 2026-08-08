package compiler

import scala.io.Source

class CompilerSuite extends munit.FunSuite {

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

        val charIter = Source.fromString(listing)
        assertEquals('\n', charIter.next())
        assertEquals('\n', charIter.ch)
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

        val compiler = new Compiler(Source.fromString(listing))

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
}