import org.scalatest.{FunSpec, Matchers}
import tokens._

class LexerTest extends FunSpec with Matchers {
  describe("Check the correct generation of Tokens") {
    describe("EOFToken") {
      describe("a program just with a comment with jump line") {
        val input = " -- Programa Vacio \n"
        it("the lexer return a EOFToken") {
          FlechaLexer(input).nextToken should equal(EOFToken())
        }
      }

      //      describe("a program just with a comment") {
      //        val input = " -- Programa Vacio"
      //        it("the lexer return a EOFToken") {
      //          FlechaLexer(input).nextToken should equal (EOFToken(List()))
      //        }
      //      }
    }
    describe("DEFToken") {
      val input = " def \n"
      it("the lexer return a DEFToken") {
        FlechaLexer(input).nextToken should equal (DEFToken())
      }
    }

    describe("LOWERIDToken") {
      val input = " uno = 2 \n"
      it("the lexer return a LOWERIDToken") {
        FlechaLexer(input).nextToken should equal (LOWERIDToken("uno"))
      }
    }

    describe("UPPERIDToken") {
      val input = " Uno = 2 \n"
      it("the lexer return a UPPERIDToken") {
        FlechaLexer(input).nextToken should equal (UPPERIDToken("Uno"))
      }
    }

    describe("DEFEQToken") {
      val input = " = 2 \n"
      it("the lexer return a DEFEQToken") {
        FlechaLexer(input).nextToken should equal (DEFEQToken())
      }
    }

    describe("NUMBERToken") {
      val input = " = 2 \n"
      it("the lexer return a NUMBERToken") {
        FlechaLexer(input).nextToken should equal (NUMBERToken(2))
      }
    }
  }
}