package ast

import play.api.libs.json.JsObject

abstract class AST { def toJson: JsObject }

abstract class ExprAST extends AST

abstract class InExprAST extends ExprAST

abstract class ExExprAST extends ExprAST

abstract class AppEAST extends ExprAST

abstract class AtomicOpAST extends AppEAST

abstract class BinaryOpAST extends InExprAST

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

case class NumberAST(value: Int) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class LowerIdAST(value: String) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class UpperIdAST(value: String) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class CharAST(value: Char) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class StringAST(value: String) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class UnaryWithParenAST(value: ExprAST) extends AtomicOpAST {
  override def toJson: JsObject = ???
}

case class MinusAST(expr: InExprAST) extends InExprAST {
  override def toJson: JsObject = ???
}

case class NotAST(expr: InExprAST) extends InExprAST {
  override def toJson: JsObject = ???
}

case class AppExprAST(atomicOp: AtomicOpAST, appExprAST: AppEAST) extends AppEAST {
  override def toJson: JsObject = ???
}

case class AndAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class OrAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class EqAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class NeAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class GeAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class LeAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class GtAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class LtAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class PlusAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class MinusBinaryAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class TimesAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class DivAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}

case class ModAST(internalExprLeft: InExprAST, internalExprRight: InExprAST) extends BinaryOpAST {
  override def toJson: JsObject = ???
}
