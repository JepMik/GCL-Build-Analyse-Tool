// This file implements a module where we define multiple data types
// -> to store, represent boolean, arithmetic, commands and guarded commands from Parser and Lexer.
// -> to store, represent predicates for program verification (parsed and further computed)
// -> to store programs with possible annotations
// -> to store edges, fragment actions
// -> to store inputs to interpreter, sign assignments to sign analysis
// -> to store security lattices, classifications and flows
module TypesAST


// ---- Type arithExpr holds basic arithmetric operations
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

// ---- Type boolExpr holds basic boolean operations
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

// ---- Type guardCommand and type command are declared together as they are 
//mutually recursive types, and they hold basic guarded commands and commands
type guardCommand = 
  | IfThen of (boolExpr * command)
  | FatBar of (guardCommand * guardCommand)
and command = 
  | Assign of (string * arithExpr) 
  | ArrayAssign of (string * arithExpr * arithExpr)
  | Skip 
  | Order of (command * command)
  | If of (guardCommand)
  | Do of (pred * guardCommand)

// ---- Type predicate holds predicates input by the user and then built 
//according to program statements (proof obligations & ShortPathFragments)
and pred = 
  | Pbool of (bool)
  | Pand of (pred * pred)
  | Por of (pred * pred)
  | Pnot of (pred)
  | Pimply of (pred * pred)
  //| EXIST of (arithExpr * Predicate)
  //| FORALL of (arithExpr * Predicate)
  | Pequal of (arithExpr * arithExpr)
  | Pnequal of (arithExpr * arithExpr)
  | Pgreater of (arithExpr * arithExpr)
  | PgreaterEqual of (arithExpr * arithExpr)
  | Pless of (arithExpr * arithExpr)
  | PlessEqual of (arithExpr * arithExpr)
  | StrP of (string)

// ---- Annotated program (begin[pred] program end[pred])
and annot = Annot of (pred * command * pred)

// ---- Edge types
type Edge = 
  | Ebool of (int * boolExpr * int) // boolean test edge
  | Ecomm of (int * command * int) // assignment edge

// ---- Input types:  
// AST for sequence of initial values for arrays
type seqInput = 
  | Seq of (arithExpr * seqInput) // [4, ...]
  | Singl of (arithExpr) // [4]

// AST for sequence of input assignments
type inputVal = 
  | SetArith of (string * arithExpr) // x=2
  | SetBool of (string * boolExpr) // b=true
  | SetArray of (string * seqInput) // A=[0,1,2]
  | SetDelim of (inputVal * inputVal) // x=3, y=0, A=[3,4]

// ---- Fragment actions:
type fragAct = 
  | B of boolExpr // boolean test
  | C of command // assignment

// ---- Sign types:
type sign = 
  | ZORO //0
  | PIKA //+
  | NARUTO //-

// AST for parsing a sequence of signs used for arrays
type signSeq = 
  | SSeq of (sign * signSeq) // {sign,...}
  | SSingl of (sign) //one single sign

//AST for parsing sequence of sign assignments x=+, y=-, ...
type signValue =
  | AUTOS // no signs provided
  | SignVariable of (string * sign) // x=+
  | SignArray of (string * signSeq) // A={+,-}
  | SignDelim of (signValue * signValue) // x=+,...,y=0

// Cheat codes right there
type cheat =
  | I of (inputVal)
  | S of (signValue)

// ---- Abstract syntax tree for security flows & classifications

type flow = (string * string)

type latticeVal =
  | AUTOL
  | LatFlow of (flow)
  | LatDelim of (latticeVal * latticeVal)

type classifVal =
  | AUTOC
  | ClassFlow of (flow)
  | ClassDelim of (classifVal * classifVal)

// type class = 
//   | Lvl of string
// [Lvl("public")]


type cheatSecure = 
  | CLS of (classifVal)
  | LAT of (latticeVal)

