import tokens._

import scala.language.postfixOps

case class FlechaLexer(var buffer: BufferedIterator[Char]) {

  def advance = buffer.next
  def isFinal = !buffer.hasNext
  def current = buffer.head

  def isJumpLine = current == '\n'
  def isWhitespace = current == ' '
  def isComments = {
    val currentString = buffer.mkString
    buffer = currentString.iterator.buffered
    currentString.startsWith("--")
  }
  def isNumber = {
    current match {
      case '0' | '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' => true
      case _                     => false
    }
  }
  def isSymbol = {
    current match {
      case '(' | ')' | ';' | '\\' | '+' | '-' |
           '*' | '/' | '%' | '=' | '&' | '!' |
           '<' | '>'             => true
      case _                     => false
    }
  }

  def removeWhitespaces = if (isWhitespace) { advance ; true } else { false }
  def removeJumpLines = if (isJumpLine) { advance ; true } else { false }
  def removeComments   = if (isComments) { advance ; advance ; advanceLine ; true } else { false }
  def advanceLine = while(!isFinal && !isJumpLine) advance

  def ignoreWhitespaceAndComments= while (!isFinal && (removeWhitespaces || removeComments || removeJumpLines)) {}

  def nextToken: Token = {
    ignoreWhitespaceAndComments

    if (isFinal) EOFToken()
    else current match {
      case '('  =>  advance; LPARENToken()                        // (
      case ')'  =>  advance; RPARENToken()                        // )
      case ';'  =>  advance; SEMICOLONToken()                     // ;
      case '\\' =>  advance; LAMBDAToken()                        // \ (lambda)
      case '+'  =>  advance; PLUSToken()                          // +
      case '*'  =>  advance; TIMESToken()                         // *
      case '/'  =>  advance; DIVToken()                           // /
      case '%'  =>  advance; MODToken()                           // %
      case '-'  =>  advance; readArrow                            //  -> or -(minus)
      case '='  =>  advance; readEqual                            //  == or =
      case '''  =>  advance; readCharacter                        // '_'
      case '\"' =>  advance; readString                           // "_"
      case '|'  =>  advance; readPipe                             // | or ||
      case '&'  =>  advance; readAnd                              // &&
      case '!'  =>  advance; readNotEq                            // != or !
      case '<'  =>  advance; readLE                               // <= or <
      case '>'  =>  advance; readGE                               // >= or >
      case '0' | '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' => readNumber                    // [0-9][0-9]*
      case  _   =>
        val id : IDToken = readID()
        id.value match {
          case "def"  => DEFToken()                               // def
          case "let"  => LETToken()                               // let
          case "then" => THENToken()                              // then
          case "case" => CASEToken()                              // case
          case "if"   => IFToken()                                // if
          case "in"   => INToken()                                // in
          case "else" => ELSEToken()                              // else
          case "elif" => ELIFToken()                              // elif
          case  _     => id                                       // some id
        }
    }
  }

  def readPipe: Token = {
    current match {
      case '|'  =>  advance; ORToken()                            // ||
      case  _   =>  PIPEToken()                                   // |
    }
  }

  def readAnd: Token = {
    current match {
      case '&'  =>  advance; ANDToken()                           // &&
      case  c   =>  error(s"&$c")                                 // Error
    }
  }

  def readLE: Token = {
    current match {
      case '='  =>  advance; LEToken()                            // <=
      case  _   =>  LTToken()                                     // <
    }
  }

  def readGE: Token = {
    current match {
      case '='  =>  advance; GEToken()                            // >=
      case  _   =>  GTToken()                                     // >
    }
  }

  def readNotEq: Token = {
    current match {
      case '='  =>  advance; NEToken()                            // !=
      case  _   =>  NOTToken()                                    // !
    }
  }

  def readArrow: Token = {
    current match {
      case '>'  =>  advance; ARROWToken()                         // ->
      case  _   =>  MINUSToken()                                  // -
    }
  }

  def readEqual: Token = {
    current match {
      case '='  =>  advance; EQToken()                            // ==
      case  _   =>  DEFEQToken()                                  // =
    }
  }

  def readCharacter: Token = {
    current match {
      case '\\'  => advance; readSpecialChar                      // '\'', '\"', '\\', '\t', '\n' or '\r'
      case  char => advance; readSimpleChar(char)                 // '_'
    }
  }

  def readSimpleChar(char: Char): Token = if(current == ''') { advance ; CHARToken(char) } else { error("Expected '") }

  def readSpecialChar: Token = {
    current match {
      case '''  => advance ; readSimpleChar(''')
      case '\"' => advance ; readSimpleChar('\"')
      case '\\' => advance ; readSimpleChar('\\')
      case 't'  => advance ; readSimpleChar('\t')
      case 'n'  => advance ; readSimpleChar('\n')
      case 'r'  => advance ; readSimpleChar('\r')
      case  _   => error("Expected a valid char")
    }
  }

  def readString: Token = {
    var string = ""
    while(!isFinal && current != '\"') { string += (if(current == '\\') { advance ; specialCharToString } else current ) ; advance }
    if(current == '\"') { advance; STRINGToken(string) }                    // "_"
    else error(s"Expected ${'\"'}")                                  // Error
  }

  def specialCharToString = {
    current match {
      case 't'  => '\t'
      case 'n'  => '\n'
      case 'r'  => '\r'
      case '''  => '\''
      case  _   => "\\" ++ current.toString
    }
  }

  def readNumber: Token = {
    var currentNumber = ""
    while(!isFinal && isNumber) { currentNumber+=current.toString ; advance }
    if(isNumber) currentNumber+=current.toString
    NUMBERToken(currentNumber.toInt)
  }

  def readID(currentString: String = "") = {
    var completeString = currentString
    while(!isFinal && !isWhitespace && !isSymbol && !isJumpLine) { completeString+=current.toString ; advance }
    if(!isWhitespace && !isSymbol && !isJumpLine) completeString+=current.toString
    if(Character.isUpperCase(completeString.charAt(0))) UPPERIDToken(completeString) else LOWERIDToken(completeString)
  }

  def error(msg: String) = throw new FlechaSyntaxError(msg)
}

class FlechaSyntaxError(val msg: String) extends Exception(s"Syntax error: $msg")
