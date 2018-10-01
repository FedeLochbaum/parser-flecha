import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

import scala.io.Source

class ParserTest  extends FunSpec with Matchers {
  describe("For each test case, check if the result of Parser is equals to output") {
    val testCases = List("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")
    val directionInput = "src/utils/testCases/input/"
    val directionOutput = "src/utils/testCases/expected/"

    testCases.foreach( testNumber =>
      it(s"Test $testNumber") {
        val input = Source.fromFile(s"${directionInput}test$testNumber.input").mkString
        val output = Source.fromFile(s"${directionOutput}test$testNumber.expected").getLines().mkString

        FlechaParser(input).parse should equal (Json.parse(output))
      }
    )

    describe("Empty program AST") {
      describe("a program just with a comment with jump line") {
        val input = " -- Programa Vacio \n "
        it("the parser return a Program AST with") {
          FlechaParser(input).parse should equal(Json.arr())
        }
      }
    }

    describe("a program just with a definition of one variable declaration") {
      val input = "def uno = 1 \n "
      it("the parser return a Program AST with definition") {
        FlechaParser(input).parse should equal(Json.arr(Json.toJson(("Def", "uno", ("ExprNumber", 1)))))
      }
    }

    describe("") {
      val input = "def\ncuatro=4--comentario \n "
      it("the parser return a Program AST") {
        FlechaParser(input).parse should equal(Json.arr(Json.toJson(("Def", "cuatro", ("ExprNumber", 4)))))
      }
    }
  }
}
