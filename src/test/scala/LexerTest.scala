import org.scalatest.{FunSpec, Matchers}
import tokens._

class LexerTest extends FunSpec with Matchers {
  describe("Check the correct generation of Tokens") {
    describe("EOFToken") {
      describe("a program just with a comment with jump line") {
        val input = " -- Programa Vacio \n ".iterator.buffered
        it("the lexer return a EOFToken") {
          FlechaLexer(input).nextToken should equal(EOFToken())
        }
      }
    }

    describe("LOWERIDToken") {
      val input = " uno = 2 \n ".iterator.buffered
      it("the lexer return a LOWERIDToken") {
        FlechaLexer(input).nextToken should equal (LOWERIDToken("uno"))
      }
    }

    describe("UPPERIDToken") {
      val input = " Uno = 2 \n ".iterator.buffered
      it("the lexer return a UPPERIDToken") {
        FlechaLexer(input).nextToken should equal (UPPERIDToken("Uno"))
      }
    }

    describe("CHARToken") {
      val input = " 'c' = 2 \n ".iterator.buffered
      it("the lexer return a CHARToken") {
        FlechaLexer(input).nextToken should equal (CHARToken("c"))
      }
    }

    describe("STRINGToken") {
      val input = " \"uno\" = 2 \n ".iterator.buffered
      it("the lexer return a STRINGToken") {
        FlechaLexer(input).nextToken should equal (STRINGToken("uno"))
      }
    }

    describe("NUMBERToken") {
      val input = " 232 \n ".iterator.buffered
      it("the lexer return a NUMBERToken") {
        FlechaLexer(input).nextToken should equal (NUMBERToken(232))
      }
    }

    describe("DEFEQToken") {
      val input = " = 2 \n ".iterator.buffered
      it("the lexer return a DEFEQToken") {
        FlechaLexer(input).nextToken should equal (DEFEQToken())
      }
    }

    describe("DEFToken") {
      val input = " def \n ".iterator.buffered
      it("the lexer return a DEFToken") {
        FlechaLexer(input).nextToken should equal (DEFToken())
      }
    }

    describe("IFToken") {
      val input = " if \n ".iterator.buffered
      it("the lexer return a IFToken") {
        FlechaLexer(input).nextToken should equal (IFToken())
      }
    }

    describe("THENToken") {
      val input = " then \n ".iterator.buffered
      it("the lexer return a THENToken") {
        FlechaLexer(input).nextToken should equal (THENToken())
      }
    }

    describe("ELIFToken") {
      val input = " elif \n ".iterator.buffered
      it("the lexer return a ELIFToken") {
        FlechaLexer(input).nextToken should equal (ELIFToken())
      }
    }

    describe("ELSEToken") {
      val input = " else \n ".iterator.buffered
      it("the lexer return a ELSEToken") {
        FlechaLexer(input).nextToken should equal (ELSEToken())
      }
    }

    describe("CASEToken") {
      val input = " case \n ".iterator.buffered
      it("the lexer return a CASEToken") {
        FlechaLexer(input).nextToken should equal (CASEToken())
      }
    }

    describe("LETToken") {
      val input = " let \n ".iterator.buffered
      it("the lexer return a LETToken") {
        FlechaLexer(input).nextToken should equal (LETToken())
      }
    }

    describe("INToken") {
      val input = " in \n ".iterator.buffered
      it("the lexer return a INToken") {
        FlechaLexer(input).nextToken should equal (INToken())
      }
    }

    describe("SEMICOLONToken") {
      val input = " ; 2 \n ".iterator.buffered
      it("the lexer return a SEMICOLONToken") {
        FlechaLexer(input).nextToken should equal (SEMICOLONToken())
      }
    }

    describe("LPARENToken") {
      val input = " ( 2 \n ".iterator.buffered
      it("the lexer return a LPARENToken") {
        FlechaLexer(input).nextToken should equal (LPARENToken())
      }
    }

    describe("RPARENToken") {
      val input = " ) 2 \n ".iterator.buffered
      it("the lexer return a RPARENToken") {
        FlechaLexer(input).nextToken should equal (RPARENToken())
      }
    }

    describe("LAMBDAToken") {
      val input = " \\ 2 \n ".iterator.buffered
      it("the lexer return a LAMBDAToken") {
        FlechaLexer(input).nextToken should equal (LAMBDAToken())
      }
    }

    describe("PIPEToken") {
      val input = " | 2 \n ".iterator.buffered
      it("the lexer return a PIPEToken") {
        FlechaLexer(input).nextToken should equal (PIPEToken())
      }
    }

    describe("ARROWToken") {
      val input = " -> 2 \n ".iterator.buffered
      it("the lexer return a ARROWToken") {
        FlechaLexer(input).nextToken should equal (ARROWToken())
      }
    }

    describe("ANDToken") {
      val input = " && 2 \n ".iterator.buffered
      it("the lexer return a ANDToken") {
        FlechaLexer(input).nextToken should equal (ANDToken())
      }
    }

    describe("ORToken") {
      val input = " || 2 \n ".iterator.buffered
      it("the lexer return a ORToken") {
        FlechaLexer(input).nextToken should equal (ORToken())
      }
    }

    describe("NOTToken") {
      val input = " ! 2 \n ".iterator.buffered
      it("the lexer return a NOTToken") {
        FlechaLexer(input).nextToken should equal (NOTToken())
      }
    }

    describe("EQToken") {
      val input = " == 2 \n ".iterator.buffered
      it("the lexer return a EQToken") {
        FlechaLexer(input).nextToken should equal (EQToken())
      }
    }

    describe("NEToken") {
      val input = " != 2 \n ".iterator.buffered
      it("the lexer return a NEToken") {
        FlechaLexer(input).nextToken should equal (NEToken())
      }
    }

    describe("GEToken") {
      val input = " >= 2 \n ".iterator.buffered
      it("the lexer return a GEToken") {
        FlechaLexer(input).nextToken should equal (GEToken())
      }
    }

    describe("LEToken") {
      val input = " <= 2 \n ".iterator.buffered
      it("the lexer return a LEToken") {
        FlechaLexer(input).nextToken should equal (LEToken())
      }
    }

    describe("GTToken") {
      val input = " > 2 \n ".iterator.buffered
      it("the lexer return a GTToken") {
        FlechaLexer(input).nextToken should equal (GTToken())
      }
    }

    describe("LTToken") {
      val input = " < 2 \n ".iterator.buffered
      it("the lexer return a LTToken") {
        FlechaLexer(input).nextToken should equal (LTToken())
      }
    }

    describe("PLUSToken") {
      val input = " + 2 \n ".iterator.buffered
      it("the lexer return a PLUSToken") {
        FlechaLexer(input).nextToken should equal (PLUSToken())
      }
    }

    describe("MINUSToken") {
      val input = " - 2 \n ".iterator.buffered
      it("the lexer return a MINUSToken") {
        FlechaLexer(input).nextToken should equal (MINUSToken())
      }
    }

    describe("TIMESToken") {
      val input = " * 2 \n ".iterator.buffered
      it("the lexer return a TIMESToken") {
        FlechaLexer(input).nextToken should equal (TIMESToken())
      }
    }

    describe("DIVToken") {
      val input = " / 2 \n ".iterator.buffered
      it("the lexer return a DIVToken") {
        FlechaLexer(input).nextToken should equal (DIVToken())
      }
    }

    describe("MODToken") {
      val input = " % 2 \n ".iterator.buffered
      it("the lexer return a MODToken") {
        FlechaLexer(input).nextToken should equal (MODToken())
      }
    }
  }
}