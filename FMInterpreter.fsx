
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

//Function that takes list of edges, and scans for variables that need value. Output is set of variables
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

// Evaluation of arithmetic expressions using the values held in memory mappings
let rec evalA e mapA arr =
  match e with
    | Num(x) -> x:float
    | TimesExpr(x,y) -> (evalA x mapA arr) * (evalA y mapA arr)
    | DivExpr(x,y) -> (evalA x mapA arr) / (evalA y mapA arr)
    | PlusExpr(x,y) -> (evalA x mapA arr) + (evalA y mapA arr) 
    | MinusExpr(x,y) -> (evalA x mapA arr) - (evalA y mapA arr)
    | PowExpr(x,y) -> (evalA x mapA arr) ** (evalA y mapA arr)
    | UPlusExpr(x) -> (evalA x mapA arr)
    | UMinusExpr(x) -> - (evalA x mapA arr)
    | LogExpr(x) -> Math.Log((evalA x mapA arr),2)
    | LnExpr(x) -> Math.Log((evalA x mapA arr))
    | StrA(x) ->
            try 
                Map.find x mapA
            with err -> 
                    printfn "ERROR: Unknown variable %s in expression." x
                    0.0
    | IndexExpr(A,x) -> 
            try 
                let vl = int (evalA x mapA arr)
                List.item vl (Map.find A arr)
            with err ->
                    printfn "ERROR: Invalid lookup of index %d in array %s" (int (evalA x mapA arr)) A
                    0.0

// Evaluation of boolean expressions using the values held in memory mappings
let rec evalB e mapB mapA arr = 
    match e with
    | Bool(x) -> x
    | ShortCircuitOr(x,y) -> (evalB x mapB mapA arr) || (evalB y mapB mapA arr)
    | ShortCircuitAnd(x,y) -> (evalB x mapB mapA arr) && (evalB y mapB mapA arr)
    | LogAnd(x,y) -> (evalB x mapB mapA arr) && (evalB y mapB mapA arr)
    | LogOr(x,y) -> (evalB x mapB mapA arr) || (evalB y mapB mapA arr)
    | Neg(x) -> not (evalB x mapB mapA arr)
    | Equal(x,y) -> (evalA x mapA arr)=(evalA y mapA arr)
    | NotEqual(x,y) -> not ((evalA x mapA arr)=(evalA y mapA arr))
    | Greater(x,y) -> (evalA x mapA arr)<(evalA y mapA arr)
    | GreaterEqual(x,y) -> (evalA x mapA arr)<=(evalA y mapA arr)
    | Less(x,y) -> (evalA x mapA arr)>(evalA y mapA arr)
    | LessEqual(x,y) -> (evalA x mapA arr)>=(evalA y mapA arr)
    | StrB(str) -> 
            try 
                Map.find str mapB
            with err -> 
                    printfn "ERROR: Unknown variable %s in expression." str
                    false


