package ast

import play.api.libs.json.JsObject

abstract class AST { def toJson: JsObject }

abstract class ExprAST extends AST

abstract class InExprAST extends ExprAST

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

case class IfAST(internalExpr: InExprAST, thenInternalExpr: InExprAST, elseInternalExpr: ExprAST) extends ExExprAST {
  override def toJson: JsObject = ???
}

case class CaseBranchAST(constructor: String, parameters: List[String], parseInternalExpression: InExprAST) extends AST {
  override def toJson: JsObject = ???
}

case class CaseAST(internalExpr: InExprAST, caseBranchs: List[CaseBranchAST]) extends ExExprAST {
  override def toJson: JsObject = ???
}

case class LetAST(name: String, parameters: List[String], internalExpr: InExprAST, externalExp: ExExprAST) extends ExExprAST {
  override def toJson: JsObject = ???
}

case class LambdaAST(parameters: List[String], externalExp: ExExprAST) extends ExExprAST {
  override def toJson: JsObject = ???
}