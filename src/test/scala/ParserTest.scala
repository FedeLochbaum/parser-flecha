import org.scalatest.{FunSpec, Matchers}
import play.api.libs.json.Json

import scala.io.Source

class ParserTest  extends FunSpec with Matchers {
  describe("For each test case, check if the result of Parser is equals to output") {
    val testCases = List("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12",
                          "13", "14", "15", "16", "17", "18")
    val directionInput = "src/utils/testCases/input/"
    val directionOutput = "src/utils/testCases/expected/"

    testCases.foreach( testNumber =>
      it(s"Test $testNumber") {
        val input = Source.fromFile(s"${directionInput}test$testNumber.input").mkString
        val output = Source.fromFile(s"${directionOutput}test$testNumber.expected").getLines().mkString

        FlechaParser(input).parse should equal (Json.parse(output))
      }
    )
  }
}
