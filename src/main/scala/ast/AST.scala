package ast

import play.api.libs.json._

abstract class AST { def toJson: JsValue }

case class ProgramAST(list: List[AST]) extends AST {
  override def toJson = Json.toJson(list.reverseMap(ast => ast.toJson))
}

case class DefAST(name: String, expression: AST) extends AST {
  override def toJson = Json.toJson("Def", name, expression.toJson)
}

case class DExprAST(externalExp: AST, exp: AST) extends AST {
  override def toJson = ???
}

case class CaseBranchAST(constructor: String, parameters: List[String], parseInternalExpression: AST) extends AST {
  override def toJson = Json.toJson("CaseBranch", constructor, parameters, parseInternalExpression.toJson)
}

case class CaseAST(internalExpr: AST, caseBranchs: List[AST]) extends AST {
  override def toJson = Json.toJson("ExprCase", internalExpr.toJson, caseBranchs.map(ast => ast.toJson))
}

case class LetAST(name: String, internalExpr: AST, externalExp: AST) extends AST {
  override def toJson = Json.toJson("ExprLet", name, internalExpr.toJson, externalExp.toJson)
}

case class LambdaAST(id: String, externalExp: AST) extends AST {
  override def toJson = Json.toJson("ExprLambda", id, externalExp.toJson)
}

case class NumberAST(value: Int) extends AST {
  override def toJson = Json.toJson("ExprNumber", value)
}

case class LowerIdAST(value: String) extends AST {
  override def toJson = Json.toJson("ExprVar", value)
}

case class UpperIdAST(value: String) extends AST {
  override def toJson = Json.toJson("ExprConstructor", value)
}

case class CharAST(value: Char) extends AST {
  override def toJson = Json.toJson("ExprChar", value.hashCode())
}

case class UnaryWithParenAST(value: AST) extends AST {
  override def toJson = value.toJson
}

case class MinusAST(expr: AST) extends AST {
  override def toJson  = ???
}

case class NotAST(expr: AST) extends AST {
  override def toJson = ???
}

case class AppExprAST(atomicOp: AST, appExprAST: AST) extends AST {
  override def toJson = Json.toJson("ExprApply", atomicOp.toJson, appExprAST.toJson)
}

case class AndAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson = ???
}

case class OrAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson = ???
}

case class EqAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson = ???
}

case class NeAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class GeAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class LeAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class GtAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class LtAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class PlusAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class MinusBinaryAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class TimesAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class DivAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}

case class ModAST(internalExprLeft: AST, internalExprRight: AST) extends AST {
  override def toJson: JsObject = ???
}
