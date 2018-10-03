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

  def isBinary = {
    currentToken match {
      case ANDToken()   | ORToken() |EQToken() | NEToken() | GEToken() | LEToken()
           | GTToken()  | LTToken() | PLUSToken() | MINUSToken() |TIMESToken()
           | DIVToken() | MODToken() => true
      case _                         => false
    }
  }

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
   val expression = createLambdaWithParams(parameters, parseExpression)
   DefAST(name, expression)
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
    while(isLowerId) { parameters = parameters.+:(parseLowerId) } ; parameters.reverse
  }

  def parseExpression: AST = {
    val externalExpr = parseExternalExpression
    if (isToken(SEMICOLONToken())) { advanceToken ; LetAST("_", externalExpr, parseExpression) } else externalExpr
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
    CaseAST(internalExpr, parseCaseBranchs)
  }

  def parseLet = {
    val name = parseLowerId
    val parameters = parseParameters
    matchToken(DEFEQToken())
    val internalExp = createLambdaWithParams(parameters, parseInternalExpression)
    matchToken(INToken())
    LetAST(name, internalExp, parseExternalExpression)
  }

  def createLambdaWithParams(parameters: List[String], expr: AST): AST = { if(parameters.isEmpty) { expr } else { LambdaAST(parameters.head, createLambdaWithParams(parameters.tail, expr)) } }

  def parseLambda = {
    val parameters = parseParameters
    matchToken(ARROWToken())
    createLambdaWithParams(parameters, parseExternalExpression)
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
    while(isToken(PIPEToken())) { advanceToken ;  branchs = branchs.+:(parseCaseBranch) } ; branchs.reverse
  }

  def parseCaseBranch = {
    val constructor = parseUpperId
    val parameters = parseParameters
    matchToken(ARROWToken())
    CaseBranchAST(constructor, parameters, parseInternalExpression)
  }

  def parseInternalExpressionAux: AST  = {
    if(isUnaryOperation) { parseUnaryOperation }
    else if (isApplicationExpression) {
      val atomic = parseAtomicOperation
      if(isBinary) { parseBinaryOperation(atomic) } else { parseApplicationExpression(atomic) }
    }
    else parseBinaryOperation()
  }

  def parseInternalExpression: AST  = {
    val internal = parseInternalExpressionAux
    if (isBinary) { AppExprAST(AppExprAST(LowerIdAST(parseBinaryOperator), internal), parseInternalExpression) } else { internal }
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

  def parseUnaryOperation = AppExprAST(LowerIdAST(parseUnaryOperator), parseInternalExpression)

  def parseUnaryOperator = {
    currentToken match {
      case MINUSToken()        => advanceToken ; "UMINUS"
      case NOTToken()          => advanceToken ; "NOT"
      case _                   => error("Some Unary Operation")
    }
  }

  def parseExpressionWithEndParen = {
    val expr = parseExpression
    matchToken(RPARENToken())
    UnaryWithParenAST(expr)
  }

  def parseApplicationExpression(atomic: AST = null) = {
    var atomicList = parseAtomics
    var appExpr = if(atomic == null) { val head::tail = atomicList ; atomicList = tail ; head } else { atomic }
    while(atomicList.nonEmpty) { appExpr = AppExprAST(appExpr, atomicList.head) ; atomicList = atomicList.tail } ; appExpr
  }

  def parseAtomics = {
    var atomics = List[AST]()
    while (isApplicationExpression) { atomics = atomics.+:(parseAtomicOperation)} ; atomics.reverse
  }

  def parsePairsOfOperationAndAtomic = {
    var list = List[(String, AST)]()
    while(isBinary) { list = list.+:((parseBinaryOperator, parseAtomicOperation)) } ; list.reverse
  }

  def parseBinaryOperation(atomic: AST = null) = {
    var pariList = parsePairsOfOperationAndAtomic
    var appExpr = if(atomic == null) { val (op, atomic)::tail = pariList ; pariList = tail ; AppExprAST(LowerIdAST(op), atomic) } else { atomic }

    while(pariList.nonEmpty) { appExpr =AppExprAST(AppExprAST(LowerIdAST(pariList.head._1), appExpr), pariList.head._2) ; pariList = pariList.tail } ; appExpr
  }

  def parseBinaryOperator = {
    currentToken match {
      case ANDToken()   => advanceToken ; "AND"
      case ORToken()    => advanceToken ; "OR"
      case EQToken()    => advanceToken ; "EQ"
      case NEToken()    => advanceToken ; "NE"
      case GEToken()    => advanceToken ; "GE"
      case LEToken()    => advanceToken ; "LE"
      case GTToken()    => advanceToken ; "GT"
      case LTToken()    => advanceToken ; "LT"
      case PLUSToken()  => advanceToken ; "ADD"
      case MINUSToken() => advanceToken ; "SUB"
      case TIMESToken() => advanceToken ; "MUL"
      case DIVToken()   => advanceToken ; "DIV"
      case MODToken()   => advanceToken ; "MOD"
      case _            => error("Some Binary Operation")
    }
  }

  def matchToken(token: Token) =  if (!isToken(token)) { error(token.toString) } else { advanceToken }

  def error(expectedType: String, extraMessage: String = "") = throw new FlechaParseError(expectedType, extraMessage, currentToken)
}

class FlechaParseError(expectedType: String, extraMessage: String, token: Token) extends Exception(s"Parse error: ExpectedType: $expectedType but found ${token.toString} $extraMessage")
