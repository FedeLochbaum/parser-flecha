import ast._
import play.api.libs.json.JsObject
import tokens._

case class FlechaParser(input : String) {

  val lexer = FlechaLexer(input.iterator.buffered)
  var currentToken: Token = lexer.nextToken
  def advanceToken = { currentToken = lexer.nextToken }

  def isToken = currentToken == _
  def isLowerId = {
    currentToken match {
      case LOWERIDToken(_) => true
      case _               => false
    }
  }

  def parse: JsObject  = {
    val definitions = List()
    while (currentToken != EOFToken()) { definitions.+:(parseDefinition) }
    ProgramAST(definitions).toJson
  }

  def parseDefinition: AST = {
    currentToken match {
      case DEFToken() => advanceToken ; parseDef
      case  _         => error("DEFToken")
    }
  }

  def parseDef: AST = {
   val name = parseLowerId
   val parameters = parseParameters
   matchToken(DEFEQToken())
   val expression = parseExpression
   DefAST(name, parameters, expression)
  }

  def parseLowerId: String = {
    currentToken match {
      case LOWERIDToken(value) => advanceToken ; value
      case _                   => error("LOWERIDToken")
    }
  }

  def parseUpperId: String = {
    currentToken match {
      case UPPERIDToken(value) => advanceToken ; value
      case _                   => error("UPPERIDToken")
    }
  }

  def parseParameters: List[String] = {
    val parameters = List()
    while(isLowerId) { parameters.+(parseLowerId) ; advanceToken } ; parameters
  }

  def parseExpression: ExpAST = {
    val externalExpr = parseExternalExpression
    if (isToken(SEMICOLONToken())) { advanceToken ; DExpAST(externalExpr, parseExpression) } else externalExpr
  }

  def parseExternalExpression: ExExpAST = {
    
  }

  def matchToken(token: Token) =  if (isToken(token)) { error(token.toString) } ; advanceToken

  def error(expectedType: String, extraMessage: String = "") = throw new FlechaParseError(expectedType, extraMessage, currentToken)
}

class FlechaParseError(expectedType: String, extraMessage: String, token: Token) extends Exception(s"Parse error: ExpectedType: $expectedType but found ${token.toString} $extraMessage")
