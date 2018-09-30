package ast

import play.api.libs.json.JsObject

abstract class AST { def toJson: JsObject }

abstract class ExprAST extends AST

abstract class InExprAST extends AST

abstract class ExExprAST extends ExprAST

case class ProgramAST(list: List[AST]) extends AST {
  override def toJson: JsObject = ???
}

case class DefAST(name: String, parameters: List[String], expression: ExprAST) extends AST {
  override def toJson: JsObject = ???
}

case class DExprAST(externalExp: ExExprAST, exp: ExprAST) extends ExprAST {
  override def toJson: JsObject = ???
}

case class IfAST(internalExpr: InExprAST, thenInternalExpr: InExprAST, elseInternalExpr: InExprAST) extends ExExprAST {
  override def toJson: JsObject = ???
}