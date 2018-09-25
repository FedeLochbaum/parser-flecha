import play.api.libs.json.Json

case class FlechaParser(input : String) {
  val lexer = FlechaLexer(input)

  def currentToken = lexer.nextToken
  def parse = {
    Json.parse("")
  }
}
