// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module CalculatorTypesAST

type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | Var of (string)
  | AccessExpr of (string * expr)

type boolexpr =
  | True
  | False
  | BitWiseAnd of (boolexpr * boolexpr)
  | BitWiseOr of (boolexpr * boolexpr)
  | LogAnd of (boolexpr * boolexpr)
  | LogOr of (boolexpr * boolexpr)
  | Neg of (boolexpr)
  | Equal of (expr * expr)
  | NotEqual of (expr * expr)
  | LeftGreater of (expr * expr)
  | LeftGreaterEqual of (expr * expr)
  | RightGreater of (expr * expr)
  | RightGreaterEqual of (expr * expr)

//type guardcommands =
//  | IfThen of (boolexpr * commands)
//  | FatBar of (guardcommands * guardcommands)

//type commands =
//  | Assign of (expr * expr)
//  | Skip
//  | Order of (commands)
//  | If of (guardcommands)
//  | Do of (guardcommands)
