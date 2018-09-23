import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

import scala.io.Source

class ParserTest  extends FunSpec with Matchers {
  val testCases = List("00", "01")
  describe("For each test case, check if the result of Parser is equals to output") {
    val directionInput = "../utils/testCases/input/"
    val directionOutput = "../utils/testCases/expected/"

    testCases.foreach( testNumber =>
      it(s"Test $testNumber") {
        val input = Source.fromFile(s"${directionInput}test$testNumber.input").getLines.mkString
        val output = Source.fromFile(s"${directionOutput}test$testNumber.expected").getLines.mkString

        FlechaParser.parse(input) should equal (Json.parse(output))
      }
    )
  }
}
