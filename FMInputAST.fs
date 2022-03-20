// This file implements a module where we define multiple data types
// to store represent boolean, arithmetic, commands and guarded commands from Parser and Lexer.
module FMInputAST

// type arithExpr does basic arithmetric operations
type arithExpr =
  | Num of int        
  | StrA of (string)
  | TimesExpr of (arithExpr * arithExpr)
  | DivExpr of (arithExpr * arithExpr)
  | PlusExpr of (arithExpr * arithExpr)
  | MinusExpr of (arithExpr * arithExpr)
  | PowExpr of (arithExpr * arithExpr)
  | UPlusExpr of (arithExpr)
  | UMinusExpr of (arithExpr)
  | LogExpr of (arithExpr)
  | LnExpr of (arithExpr)
  | IndexExpr of (string * arithExpr)

// type boolExpr does basic bool operations
type boolExpr = 
  | Bool of (bool)
  | StrB of (string)
  | ShortCircuitAnd of (boolExpr * boolExpr)
  | ShortCircuitOr of (boolExpr * boolExpr)
  | LogAnd of (boolExpr * boolExpr)
  | LogOr of (boolExpr * boolExpr)
  | Neg of (boolExpr)
  | Equal of (arithExpr * arithExpr)
  | NotEqual of (arithExpr * arithExpr)
  | Greater of (arithExpr * arithExpr)
  | GreaterEqual of (arithExpr * arithExpr)
  | Less of (arithExpr * arithExpr)
  | LessEqual of (arithExpr * arithExpr)


type seqInput = 
  | Seq of (arithExpr * List<arithExpr> ) 
  
type inputVal = 
  | SetArith of (string * arithExpr)
  | SetBool of (string * boolExpr)
  | SetArray of (string * seqInput) 
  | SetDelim of (inputVal * inputVal)
