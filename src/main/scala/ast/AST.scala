package ast

import play.api.libs.json.JsObject

abstract class AST { def toJson: JsObject }

abstract class ExpAST extends AST

case class ProgramAST(list: List[AST]) extends AST {
  override def toJson: JsObject = ???
}

case class DefAST(name: String, parameters: List[String], expression: ExpAST) extends AST {
  override def toJson: JsObject = ???
}

case class ExExpAST() extends ExpAST {
  override def toJson: JsObject = ???
}

case class DExpAST(externalExp: ExExpAST, exp: ExpAST) extends ExpAST {
  override def toJson: JsObject = ???
}
