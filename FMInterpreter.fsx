
#load "FMInputAST.fs"
open FMInputAST

#load "FMProjectTypesAST.fs"
open FMProjectTypesAST
open System

//Function that looks into arithmetic and generates a set of variables.
let rec peekArith expr =
    match expr with
    | StrA(x) -> Set.empty.Add(x)
    | Num(x) -> Set.empty
    | TimesExpr(x,y) -> Set.union (peekArith x) (peekArith y)
    | DivExpr(x,y) -> Set.union (peekArith x) (peekArith y)
    | PlusExpr(x,y) -> Set.union (peekArith x) (peekArith y)
    | MinusExpr(x,y) -> Set.union (peekArith x) (peekArith y)
    | PowExpr(x,y) -> Set.union (peekArith x) (peekArith y)
    | UPlusExpr(x) -> peekArith x
    | UMinusExpr(x) -> peekArith x
    | LogExpr(x) -> peekArith x
    | LnExpr(x) -> peekArith x
    | IndexExpr(A,x) ->  (peekArith x).Add(A)

//Function looks into bool and generates a set of variables
let rec peekBool expr =
    match expr with
    | StrB(x) -> Set.empty.Add(x)
    | Bool(x) -> Set.empty
    | ShortCircuitAnd(x,y) -> Set.union (peekBool x) (peekBool y)
    | ShortCircuitOr(x,y) -> Set.union (peekBool x) (peekBool y)
    | LogAnd(x,y) -> Set.union (peekBool x) (peekBool y)
    | LogOr(x,y) -> Set.union (peekBool x) (peekBool y)
    | Neg(x) -> (peekBool x)
    | Equal(x,y) -> Set.union (peekArith x) (peekArith y)
    | NotEqual(x,y) -> Set.union (peekArith x) (peekArith y)
    | Greater(x,y) -> Set.union (peekArith x) (peekArith y)
    | GreaterEqual(x,y) -> Set.union (peekArith x) (peekArith y)
    | Less(x,y) -> Set.union (peekArith x) (peekArith y)
    | LessEqual(x,y) -> Set.union (peekArith x) (peekArith y)
    

let rec peekCommand com = 
    match com with
        | ArrayAssign(arr,index,value) -> Set.union ((peekArith index).Add(arr)) (peekArith value)
        | Assign(var,value) -> (peekArith value).Add(var)
        | _ -> Set.empty ;

//Function that takes list of edges, and scans for variables that needs value. Output is set of variables
let rec varAFinder edges =
    match edges with
    | Ecomm(_,com,_)::tail -> Set.union (peekCommand com) (varAFinder tail)
    | [] | _ -> Set.empty
        
let rec varBFinder edges = 
    match edges with 
    | Ebool(_,bool,_)::tail -> Set.union (peekBool bool) (varBFinder tail) 
    | [] | _ -> Set.empty

// Initialize one arithmetic variable name to value 0 - helper for folding
let varAInit item (map:Map<string,int>) = map.Add(item, 0)
// Initialize the mapping of all arithmetic variables in given set to 0
let initAllAVar (set:Set<string>) = Set.foldBack (fun item map ->  varAInit item map) set Map.empty
// initAllAVar (varAFinder edges)

// Initialize one arithmetic variable name to value 0 - helper for folding
let varBInit item (map:Map<string,bool>) = map.Add(item, false)
// Initialize the mapping of all arithmetic variables in given set to 0
let initAllBVar (set:Set<string>) = Set.foldBack (fun item map ->  varBInit item map) set Map.empty
// initAllBVar (varBFinder edges)

// let rec evalA e mapA =
//   match e with
//     | Num(x) -> x:float
//     | TimesExpr(x,y) -> (evalA x mapA) * (evalA y mapA)
//     | DivExpr(x,y) -> evalA(x) / evalA (y)
//     | PlusExpr(x,y) -> evalA(x) + evalA (y)
//     | MinusExpr(x,y) -> evalA(x) - evalA (y)
//     | PowExpr(x,y) -> evalA(x) ** evalA (y)
//     | UPlusExpr(x) -> evalA(x)
//     | UMinusExpr(x) -> - evalA(x)
//     | LogExpr(x) -> Math.Log(evalA(x),2)
//     | LnExpr(x) -> Math.Log(evalA(x))
//     | Str
//     | _ -> 0.0 //#TODO

// let rec evalB e = 
//     match e with
//     | Bool(x) -> x
//     | 




