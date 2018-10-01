import ast.{DefAST, NumberAST, ProgramAST}
import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

import scala.io.Source

class ParserTest  extends FunSpec with Matchers {
  describe("For each test case, check if the result of Parser is equals to output") {
    val testCases = List("00", "01")
    val directionInput = "src/utils/testCases/input/"
    val directionOutput = "src/utils/testCases/expected/"

    testCases.foreach( testNumber =>
      it(s"Test $testNumber") {
        val input = Source.fromFile(s"${directionInput}test$testNumber.input").getLines.mkString
        val output = Source.fromFile(s"${directionOutput}test$testNumber.expected").getLines.mkString

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
      val definition = DefAST("uno", List(), NumberAST(1))
      it("the parser return a Program AST with definition") {
        FlechaParser(input).parse should equal(ProgramAST(List(definition)))
      }
    }
  }
}
