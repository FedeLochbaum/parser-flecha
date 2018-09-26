import tokens._

import scala.language.postfixOps

case class FlechaLexer(input: String) {
  var buffer: BufferedIterator[Char] = input.iterator.buffered

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
      case 'd'  =>  advance; readDef                              // def or (some id)
      case 'l'  =>  advance; readLet                              // let or (some id)
      case 't'  =>  advance; readThen                             // then or (some id)
      case 'c'  =>  advance; readCase                             // case or (some id)
      case 'i'  =>  advance; readIfOrIn                           // if, in or (some id)
      case 'e'  =>  advance; readElseOrElif                       // else, elif or (some id)
      case '0' | '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' => readNumber                    // [0-9][0-9]*
      case _    => EOFToken(s"Unrecognized start of token: $current")
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
      case  _   =>  EOFToken(s"Syntax error: &${_}")              // Error
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
      case '\"'  => advance; ReadSpecialChar                      // '\'', '\"', '\\', '\t', '\n' or '\r'
      case  _  =>   advance; readSimpleChar(_)                    // '_'
    }
  }

  def readString: Token = {
    val string = ""
    while(!isFinal && buffer.next() != '"') { string.concat(current.toString)}
    if(isFinal) EOFToken(s"Syntax error: expected ${'\"'}")                     // Error
    else STRINGToken(string)                                                    // "_"
  }
}
