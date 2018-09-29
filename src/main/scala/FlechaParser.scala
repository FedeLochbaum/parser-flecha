import play.api.libs.json.Json
import tokens.{DEFToken, EOFToken, Token}

case class FlechaParser(input : String) {
  val lexer = FlechaLexer(input.iterator.buffered)
  advanceToken

  var currentToken : Token
  def advanceToken = { currentToken = lexer.nextToken }

  def parse = {
    var definitions = List()
    while (currentToken != EOFToken()) { definitions.+:(parseDefinition) }
    ProgramAST(definitions)
  }

  def parseDefinition = {
    currentToken match {
      case DEFToken() => parseDEF
      case  _         => error("Definition")
    }
  }
  def error(expectedType: String) = throw new FlechaParseError(expectedType, currentToken)
}

class FlechaParseError(val expectedType: String, token: Token) extends Exception(s"Parse error: ExpectedType: $expectedType but found ${token.toString}")