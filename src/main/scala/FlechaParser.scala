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

  def parseDef = {
   val name = parseLowerId
   val parameters = parseParameters
   matchToken(DEFEQToken())
   val expression = parseExpression
   DefAST(name, parameters, expression)
  }

  def parseLowerId = {
    currentToken match {
      case LOWERIDToken(value) => advanceToken ; value
      case _                   => error("LOWERIDToken")
    }
  }

  def parseUpperId = {
    currentToken match {
      case UPPERIDToken(value) => advanceToken ; value
      case _                   => error("UPPERIDToken")
    }
  }

  def parseParameters: List[String] = {
    val parameters = List()
    while(isLowerId) { parameters.+:(parseLowerId) ; advanceToken } ; parameters
  }

  def parseExpression: ExprAST = {
    val externalExpr = parseExternalExpression
    if (isToken(SEMICOLONToken())) { advanceToken ; DExprAST(externalExpr, parseExpression) } else externalExpr
  }

  def parseExternalExpression: ExExprAST = {
    currentToken match {
      case IFToken()     => advanceToken ; parseIf
      case CASEToken()   => advanceToken ; parseCase
      case LETToken()    => advanceToken ; parseLet
      case LAMBDAToken() => advanceToken ; parseLambda
      case _             => parseInternalExpression
    }
  }

  def parseIf: ExExprAST = {
    val internalExpr = parseInternalExpression
    matchToken(THENToken())
    val thenInternalExpr = parseInternalExpression
    IfAST(internalExpr, thenInternalExpr, parseElse)
  }

  def parseCase: ExExprAST = {
    val internalExpr = parseInternalExpression
    CaseAST(internalExpr, parseCaseBranchs)
  }

  def parseLet: ExExprAST = {
    val name = parseLowerId
    val parameters = parseParameters
    matchToken(DEFEQToken())
    val internalExp = parseInternalExpression
    matchToken(INToken())
    LetAST(name, parameters, internalExp, parseExternalExpression)
  }

  def parseLambda: ExExprAST = {
    val parameters = parseParameters
    matchToken(ARROWToken())
    LambdaAST(parameters, parseExternalExpression)
  }

  def parseElse = {
    currentToken match {
      case ELIFToken()     => advanceToken ; parseIf
      case ELSEToken()     => advanceToken ; parseInternalExpression
      case _               => error("ELSEToken or ELIFToken")
    }
  }

  def parseCaseBranchs = {
    val branchs = List()
    while(isToken(PIPEToken)) { advanceToken ;  branchs.+:(parseCaseBranch) } ; branchs
  }

  def parseCaseBranch = {
    val constructor = parseUpperId
    val parameters = parseParameters
    matchToken(ARROWToken())
    CaseBranchAST(constructor, parameters, parseInternalExpression)
  }

  def parseInternalExpression : InExprAST = {

  }

  def matchToken(token: Token) =  if (isToken(token)) { error(token.toString) } ; advanceToken

  def error(expectedType: String, extraMessage: String = "") = throw new FlechaParseError(expectedType, extraMessage, currentToken)
}

class FlechaParseError(expectedType: String, extraMessage: String, token: Token) extends Exception(s"Parse error: ExpectedType: $expectedType but found ${token.toString} $extraMessage")
