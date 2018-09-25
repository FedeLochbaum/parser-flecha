import play.api.libs.json.Json

case class FlechaParser(input : String) {
  val lexer = FlechaLexer(input.iterator.buffered)

  def currentToken = lexer.nextToken
  def parse = {
    Json.parse("")
  }
}
