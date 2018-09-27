import tokens._

import scala.language.postfixOps

case class FlechaLexer(input: String) {
  var buffer = input.iterator.buffered

  def advance =  buffer next
  def isFinal = !buffer.hasNext
  def current = buffer.head

  def isJumpLine = current == '\n'
  def isWhitespace = ' ' == current
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
  def advanceLine: Unit = if (!isFinal) advance ; while(!isFinal && !isJumpLine) advance ; while (isJumpLine) advance

  def ignoreWhitespaceAndComments= while (!isFinal && (removeWhitespaces || removeComments)) {}

  def nextToken: Token = {
    ignoreWhitespaceAndComments

    if (isFinal) EOFToken()
    else current match {
      case '('  =>  advance; LPARENToken()                        // (
      case ')'  =>  advance; RPARENToken()                        // )
      case ';'  =>  advance; SEMICOLONToken()                     // ;
      case '\"' =>  advance; LAMBDAToken()                        // \ (lambda)
      case '+'  =>  advance; PLUSToken()                          // +
      case '*'  =>  advance; TIMESToken()                         // *
      case '/'  =>  advance; DIVToken()                           // /
      case '%'  =>  advance; MODToken()                           // %
      case '-'  =>  advance; readArrow                            //  -> or -(minus)
      case '='  =>  advance; readEqual                            //  == or =
      case '''  =>  advance; readCharacter                        // '_'
      case '"'  =>  advance; readString                           // "_"
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
      case  c   =>  error(s"Syntax error: &$c")            // Error
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
      case '\"'  => advance; readSpecialChar                      // '\'', '\"', '\\', '\t', '\n' or '\r'
      case  char => advance; readSimpleChar(char.toString)        // '_'
    }
  }

  def readSimpleChar(char: String): Token = if(current == ''') CHARToken(char) else error(s"Syntax error: expected '")

  def readSpecialChar: Token = {
    current match {
      case ''' | '"' | '\"' | 't' | 'n' | 'r'  => readSimpleChar("\"".concat(current.toString))
      case _                                   => error(s"Syntax error: expected a valid char'")
    }
  }

  def readString: Token = {
    val string = ""
    while(!isFinal && buffer.next() != '"') { string.concat(current.toString)}
    if(isFinal) error(s"Syntax error: expected ${'\"'}")                  // Error
    else STRINGToken(string)                                                    // "_"
  }

  def readNumber: Token = {
//    var currentNumber = ""
//    while(!isFinal && isNumber) currentNumber+=current.toString ; advance
//    if(isFinal) {
//      if(isWhitespace)
//        NUMBERToken(currentNumber.toInt)
//      else {
//        if(isNumber)  NUMBERToken(currentNumber.concat(current.toString).toInt)
//      }
//    }
    // TODO: MISSING IMPLEMENTATION
    NUMBERToken(0)
  }

  def readKeyWord(word: String, token: Token): Token = {
    var currentString = ""
    while(!isFinal && word.contains(currentString ++ current.toString)) currentString+=current.toString ; advance
    if(currentString == word && current == ' ') token else readID(currentString)
  }

  def readID(currentString: String = ""): Token = {
    // TODO: MISSING IMPLEMENTATION
    error(current)
  }

  def error(msg: String) = throw new MalformedInput(msg)
}

class MalformedInput(val msg: String) extends Exception(s"Malformed input: $msg")
