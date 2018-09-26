import java.io.StringReader

import tokens._

import scala.language.postfixOps

case class FlechaLexer(input: String) {

  var buffer: BufferedIterator[Char] = input.iterator.buffered

  def errors = List()

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
      case '-'  =>  readArrow                                     //  -> -> or -(minus)
      case '='  =>  readEqual                                     //  == or =
      case '''  =>  readCharacter                                 // '_'
      case '"'  =>  readString                                    // "_"
      case '|'  =>  readPipe                                      // | or || advance; PIPEToken()
      case '&'  =>  readAnd                                       // &&
      case '!'  =>  readNotEq                                     // != or !
      case '<'  =>  readLE                                        // <= or <
      case '>'  =>  readGE                                        // >= or >
      case 'd'  =>  readDef                                       // def or (some id)
      case 'l'  =>  readLet                                       // let or (some id)
      case 't'  =>  readThen                                      // then or (some id)
      case 'c'  =>  readCase                                      // case or (some id)
      case 'i'  =>  readIfOrIn                                    // if, in or (some id)
      case 'e'  =>  readElseOrElif                                // else, elif or (some id)
      case '0' | '1' | '2' | '3' | '4' | '5' |
           '6' | '7' | '8' | '9' => readNumber                    // [0-9][0-9]*
      case _    => EOFToken(List(s"Unrecognized start of token: $current"))
    }
  }
}
