import ast._
import play.api.libs.json.{JsObject, JsValue}
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

  def isUpperId = {
    currentToken match {
      case UPPERIDToken(_) => true
      case _               => false
    }
  }

  def isNumber = {
    currentToken match {
      case NUMBERToken(_)  => true
      case _               => false
    }
  }

  def isChar = {
    currentToken match {
      case CHARToken(_)    => true
      case _               => false
    }
  }

  def isString = {
    currentToken match {
      case STRINGToken(_)  => true
      case _               => false
    }
  }

  def isUnaryOperation = {
    currentToken match {
      case NOTToken() | MINUSToken() => true
      case _                         => false
    }
  }

  def isApplicationExpression = isLowerId || isUpperId || isNumber || isChar || isString || isToken(LPARENToken())

  def resetLexer = { lexer.buffer = input.iterator.buffered ; advanceToken }

  def parse: JsValue  = {
    resetLexer
    var definitions = List[AST]()
    while (currentToken != EOFToken()) { definitions = definitions.+:(parseDefinition)}
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

  def parseParameters = {
    var parameters = List[String]()
    while(isLowerId) { parameters = parameters.+:(parseLowerId) } ; parameters
  }

  def parseExpression: AST = {
    val externalExpr = parseExternalExpression
    if (isToken(SEMICOLONToken())) { advanceToken ; DExprAST(externalExpr, parseExpression) } else externalExpr
  }

  def parseExternalExpression: AST = {
    currentToken match {
      case IFToken()     => advanceToken ; parseIf
      case CASEToken()   => advanceToken ; parseCase
      case LETToken()    => advanceToken ; parseLet
      case LAMBDAToken() => advanceToken ; parseLambda
      case _             => parseInternalExpression
    }
  }

  def parseIf = {
    val internalExpr = parseInternalExpression
    matchToken(THENToken())
    val thenCase = CaseBranchAST("True", List(), parseInternalExpression)
    val elseCase = CaseBranchAST("False", List(), parseElse)
    CaseAST(internalExpr, List(thenCase, elseCase))
  }

  def parseCase = {
    val internalExpr = parseInternalExpression
    CaseAST(internalExpr, parseCaseBranchs.reverse)
  }

  def parseLet = {
    val name = parseLowerId
    val parameters = parseParameters
    matchToken(DEFEQToken())
    val internalExp = parseInternalExpression
    matchToken(INToken())
    LetAST(name, parameters, internalExp, parseExternalExpression)
  }

  def parseLambda = {
    val parameters = parseParameters
    matchToken(ARROWToken())
    LambdaAST(parameters, parseExternalExpression)
  }

  def parseElse: AST = {
    currentToken match {
      case ELIFToken()     => advanceToken ; parseIf
      case ELSEToken()     => advanceToken ; parseInternalExpression
      case _               => error("ELSEToken or ELIFToken")
    }
  }

  def parseCaseBranchs = {
    var branchs = List[AST]()
    while(isToken(PIPEToken())) { advanceToken ;  branchs = branchs.+:(parseCaseBranch) } ; branchs
  }

  def parseCaseBranch = {
    val constructor = parseUpperId
    val parameters = parseParameters.reverse
    matchToken(ARROWToken())
    CaseBranchAST(constructor, parameters, parseInternalExpression)
  }

  def parseInternalExpression: AST  = {
    if(isUnaryOperation) { parseUnaryOperation }
    else if (isApplicationExpression) { parseApplicationExpression }
    else parseBinaryOperation
  }

  def parseAtomicOperation  = {
    currentToken match {
      case LOWERIDToken(value) => advanceToken ; LowerIdAST(value)
      case UPPERIDToken(value) => advanceToken ; UpperIdAST(value)
      case NUMBERToken(value)  => advanceToken ; NumberAST(value)
      case CHARToken(value)    => advanceToken ; CharAST(value)
      case STRINGToken(value)  => advanceToken ; parseString(value)
      case LPARENToken()       => advanceToken ; parseExpressionWithEndParen
      case _                   => error("Some Atomic Operation")
    }
  }

  def parseString(string: String): AST = {
    if(string.isEmpty) { UpperIdAST("Nil") }
    else AppExprAST(AppExprAST(UpperIdAST("Cons"), CharAST(string.head)), parseString(string.tail))
  }

  def parseUnaryOperation = {
    currentToken match {
      case MINUSToken()        => advanceToken ; MinusAST(parseInternalExpression)
      case NOTToken()          => advanceToken ; NotAST(parseInternalExpression)
      case _                   => error("Some Unary Operation")
    }
  }

  def parseExpressionWithEndParen = {
    val expr = parseExpression
    matchToken(RPARENToken())
    UnaryWithParenAST(expr)
  }

  def parseApplicationExpression = {
    var atomicList = parseAtomics.reverse
    var appExpr = atomicList.head
    atomicList = atomicList.tail
    while(atomicList.nonEmpty) { appExpr = AppExprAST(appExpr, atomicList.head) ; atomicList = atomicList.tail } ; appExpr
  }

  def parseAtomics = {
    var atomics = List[AST]()
    while (isApplicationExpression) { atomics = atomics.+:(parseAtomicOperation)}
    atomics
  }

  def parseBinaryOperation = {
    val internalExpr = parseInternalExpression
    currentToken match {
      case ANDToken()   => AndAST(internalExpr, parseInternalExpression)
      case ORToken()    => OrAST(internalExpr, parseInternalExpression)
      case EQToken()    => EqAST(internalExpr, parseInternalExpression)
      case NEToken()    => NeAST(internalExpr, parseInternalExpression)
      case GEToken()    => GeAST(internalExpr, parseInternalExpression)
      case LEToken()    => LeAST(internalExpr, parseInternalExpression)
      case GTToken()    => GtAST(internalExpr, parseInternalExpression)
      case LTToken()    => LtAST(internalExpr, parseInternalExpression)
      case PLUSToken()  => PlusAST(internalExpr, parseInternalExpression)
      case MINUSToken() => MinusBinaryAST(internalExpr, parseInternalExpression)
      case TIMESToken() => TimesAST(internalExpr, parseInternalExpression)
      case DIVToken()   => DivAST(internalExpr, parseInternalExpression)
      case MODToken()   => ModAST(internalExpr, parseInternalExpression)
      case _            => error("Some Binary Operation")
    }
  }

  def matchToken(token: Token) =  if (!isToken(token)) { error(token.toString) } else { advanceToken }

  def error(expectedType: String, extraMessage: String = "") = throw new FlechaParseError(expectedType, extraMessage, currentToken)
}

class FlechaParseError(expectedType: String, extraMessage: String, token: Token) extends Exception(s"Parse error: ExpectedType: $expectedType but found ${token.toString} $extraMessage")
