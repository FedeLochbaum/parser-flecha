import tokens.{ELSEToken, EOFToken, THENToken, Token}

case class FlechaLexer(var buffer: BufferedIterator[Char]) {

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
  def advanceLine: Unit = if (!isFinal) advance ; while(!isFinal && !isJumpLine) advance ; if (isJumpLine) advance

  def ignoreWhitespaceAndComments= while (!isFinal && (removeWhitespaces || removeComments)) {}

  def nextToken: Token = {
    ignoreWhitespaceAndComments

    if(isFinal) EOFToken(errors) else {
      println(current)
      current match {
        case _ => ELSEToken()
      }
    }
  }

}
