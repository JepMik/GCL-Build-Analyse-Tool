// This file implements a module where we define a data type "expr"
// to store represent arithmetic expressions
module FMProjectTypesAST

type expr =
  | Num of float
  | TimesExpr of (expr * expr)
  | DivExpr of (expr * expr)
  | PlusExpr of (expr * expr)
  | MinusExpr of (expr * expr)
  | PowExpr of (expr * expr)
  | UPlusExpr of (expr)
  | UMinusExpr of (expr)
  | LogExpr of (expr * expr)
  | LnExpr of (expr)
  | RootExpr of (expr)
  | Var of (string)
  | IndexExpr of (string * expr)

type boolExpr = 
  | True
  | False
  | BitWiseAnd of (boolExpr * boolExpr)
  | BitWiseOr of (boolExpr * boolExpr)
  | LogAnd of (boolExpr * boolExpr)
  | LogOr of (boolExpr * boolExpr)
  | Neg of (boolExpr)
  | Equal of (expr * expr)
  | NotEqual of (expr * expr)
  | Greater of (expr * expr)
  | GreaterEqual of (expr * expr)
  | Less of (expr * expr)
  | LessEqual of (expr * expr)

type guardCommand = 
  | IfThen of (boolExpr * command)
  | FatBar of (guardCommand * guardCommand)
and command = 
  | Assign of (string * expr)
  | Skip 
  | Order of (command)
  | If of (guardCommand)
  | Do of (guardCommand)
