import tokens._

import scala.language.postfixOps

case class FlechaLexer(input: String) {
  var buffer: BufferedIterator[Char] = input.iterator.buffered

  def advance =  buffer.next
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

  def removeWhitespaces = if (isWhitespace) { advance ; true } else { false }
  def removeComments   = if (isComments) { advanceLine ; true } else { false }
  def advanceLine = if (!isFinal) advance ; while(!isFinal && !isJumpLine) advance ; while (isJumpLine && buffer.hasNext) advance

  def ignoreWhitespaceAndComments= while (!isFinal && (removeWhitespaces || removeComments)) {}

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
      case '\"'  =>  advance; readString                           // "_"
      case '|'  =>  advance; readPipe                             // | or ||
      case '&'  =>  advance; readAnd                              // &&
      case '!'  =>  advance; readNotEq                            // != or !
      case '<'  =>  advance; readLE                               // <= or <
      case '>'  =>  advance; readGE                               // >= or >
      case 'd'  =>  readKeyWord("def",  DEFToken())               // def or (some id)
      case 'l'  =>  readKeyWord("let",  LETToken())               // let or (some id)
      case 't'  =>  readKeyWord("then", THENToken())              // then or (some id)
      case 'c'  =>  readKeyWord("case", CASEToken())              // case or (some id)
      case 'i'  =>  advance; readIfOrIn                           // if, in or (some id)
      case 'e'  =>  advance; readElseOrElif                       // else, elif or (some id)
      case '0' | '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' => readNumber                    // [0-9][0-9]*
      case  _   => readID()                                       // Id or Error -> (s"Unrecognized start of token: $current")
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

  def readIfOrIn: Token = {
    current match {
      case 'f'  =>  advance; IFToken()                            // if
      case 'n'  =>  advance; INToken()                            // in
      case  _   =>  readID("i")                                   // Id
    }
  }

  def readElseOrElif: Token = {
    if(current != 'l') {
      if(!isFinal && buffer.next() == 'i') {
        readKeyWord("if", ELIFToken())
      } else if(!isFinal && buffer.next() == 's') {
        readKeyWord("se",  ELSEToken())
      } else readID("el")
    } else readID("e")
  }

  def readCharacter: Token = {
    current match {
      case '\\'  => advance; readSpecialChar                      // '\'', '\"', '\\', '\t', '\n' or '\r'
      case  char => advance; readSimpleChar(char.toString)        // '_'
    }
  }

  def readSimpleChar(char: String): Token = if(current == ''') { advance ; CHARToken(char) } else error("Expected '")

  def readSpecialChar: Token = {
    current match {
      case ''' | '"' | '\\' | 't' | 'n' | 'r'  => readSimpleChar("\\".concat(current.toString))
      case _                                   => error("Expected a valid char")
    }
  }

  def readString: Token = {
    val string = ""
    while(!isFinal && current != '"') { advance ; string.concat(current.toString) }
    if(current == '"') { advance; STRINGToken(string) }                    // "_"
    else error(s"Expected ${'\"'}")                                 // Error
  }

  def readNumber: Token = {
    var currentNumber = ""
    while(!isFinal && isNumber) { currentNumber+=current.toString ; advance }
    if(isNumber) currentNumber+=current.toString
    NUMBERToken(currentNumber.toInt)
  }

  def readKeyWord(word: String, token: Token): Token = {
    var currentString = ""
    while(!isFinal && word.contains(currentString ++ current.toString)) { currentString+=current.toString ; advance }
    if(word.contains(currentString ++ current.toString)) currentString+=current.toString
    if(currentString == word && (isWhitespace || isFinal)) token else readID(currentString)
  }

  def readID(currentString: String = ""): Token = {
    var completeString = currentString
    while(!isFinal && !isWhitespace) { completeString+=current.toString ; advance }
    if(!isWhitespace) completeString+=current.toString
    if(Character.isUpperCase(completeString.charAt(0))) UPPERIDToken(completeString) else LOWERIDToken(completeString)
  }

  def error(msg: String) = throw new MalformedInput(msg)
}

class MalformedInput(val msg: String) extends Exception(s"Syntax error: $msg")
