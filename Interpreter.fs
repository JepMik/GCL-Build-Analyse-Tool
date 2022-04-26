module Interpreter

//#load "FMProgramGraph.fs"
open ProgramGraph
//#load "FMProjectTypesAST.fs"
open TypesAST
open System

//Function that looks into arithmetic and generates a set of variables/set of array names.
let rec peekArith expr =
    match expr with
    | StrA(x) -> (Set.empty.Add(x), Set.empty)
    | Num(x) -> (Set.empty, Set.empty)
    | TimesExpr(x,y) -> 
                    let (setVx,setAx) = peekArith x
                    let (setVy,setAy) = peekArith y
                    (Set.union setVx setVy, Set.union setAx setAy)
    | DivExpr(x,y) -> 
                    let (setVx,setAx) = peekArith x
                    let (setVy,setAy) = peekArith y
                    (Set.union setVx setVy, Set.union setAx setAy)
    | PlusExpr(x,y) -> 
                    let (setVx,setAx) = peekArith x
                    let (setVy,setAy) = peekArith y
                    (Set.union setVx setVy, Set.union setAx setAy)
    | MinusExpr(x,y) -> 
                    let (setVx,setAx) = peekArith x
                    let (setVy,setAy) = peekArith y
                    (Set.union setVx setVy, Set.union setAx setAy)
    | PowExpr(x,y) -> 
                    let (setVx,setAx) = peekArith x
                    let (setVy,setAy) = peekArith y
                    (Set.union setVx setVy, Set.union setAx setAy)
    | UPlusExpr(x) -> (peekArith x)
    | UMinusExpr(x) -> (peekArith x)
    | LogExpr(x) -> (peekArith x)
    | LnExpr(x) -> (peekArith x)
    | IndexExpr(A,x) ->  
                let (setV, setA) = peekArith x    
                (setV, Set.add A setA)

//Function looks into bool and generates a set of variables
let rec peekBool expr =
    match expr with
    | StrB(x) -> (Set.empty.Add(x), Set.empty, Set.empty)
    | Bool(x) -> (Set.empty, Set.empty, Set.empty)
    | ShortCircuitAnd(x,y) -> 
                        let (b1,a1,arr1) = peekBool x
                        let (b2,a2,arr2) = peekBool y
                        (Set.union b1 b2 , Set.union a1 a2, Set.union arr1 arr2)
    | ShortCircuitOr(x,y) -> 
                        let (b1,a1,arr1) = peekBool x
                        let (b2,a2,arr2) = peekBool y
                        (Set.union b1 b2 , Set.union a1 a2, Set.union arr1 arr2)
    | LogAnd(x,y) -> 
                        let (b1,a1,arr1) = peekBool x
                        let (b2,a2,arr2) = peekBool y
                        (Set.union b1 b2 , Set.union a1 a2, Set.union arr1 arr2)
    | LogOr(x,y) -> 
                        let (b1,a1,arr1) = peekBool x
                        let (b2,a2,arr2) = peekBool y
                        (Set.union b1 b2 , Set.union a1 a2, Set.union arr1 arr2)
    
    | Neg(x) -> (peekBool x)
    | Equal(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    | NotEqual(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    | Greater(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    | GreaterEqual(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    | Less(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    | LessEqual(x,y) -> 
                let (setAx, setArrx) = peekArith x
                let (setAy, setArry) = peekArith y
                (Set.empty, Set.union setAx setAy, Set.union setArrx setArry)
    
// Function to look into commands and find variables
let rec peekCommand com = 
    match com with
        | ArrayAssign(arr,index,value) -> 
                            let (setA, setArr) = peekArith index
                            let (setA2, setArr2) = peekArith value
                            (Set.union setA setA2, Set.add arr (Set.union setArr setArr2)) 
        | Assign(var,value) -> 
                            let (setA, setArr) = peekArith value
                            (Set.add var setA, setArr) //Maybe var not needed to be included
        | _ -> (Set.empty, Set.empty) ;

let rec varAFinder edges =
    match edges with
    | Ecomm(_,com,_)::tail -> 
                    let (setA, setAr) = peekCommand com
                    let (setAx, setArx) = varAFinder tail
                    (Set.union setA setAx, Set.union setAr setArx)
    | _::tail -> varAFinder tail
    | [] -> (Set.empty, Set.empty)
        
let rec varBFinder edges = 
    match edges with 
    | Ebool(_,bool,_)::tail -> 
                            let (setB, setA, setArr) = peekBool bool
                            let (setB', setA', setArr') = varBFinder tail
                            (Set.union setB setB', Set.union setA setA', Set.union setArr setArr')
    | _::tail -> varBFinder tail 
    | [] -> (Set.empty, Set.empty, Set.empty)

//Function that takes list of edges, and scans for variables that need value. Output is set of variables
let varAllFinder edges = 
    let (setB1, setA1, setArr) = varBFinder edges 
    let (setA2, setArr2) = varAFinder edges
    (setB1, Set.union setA1 setA2,Set.union setArr setArr2)

// Initialize the mapping of all arithmetic variables in given set to 0
let varAInit item (map:Map<string,float>) = map.Add(item, 0.0)
let initAllAVar (set:Set<string>) = Set.foldBack (fun item map ->  varAInit item map) set Map.empty
// initAllAVar (varAFinder edges)

// Initialize the mapping of all array variables in given set to 0
let varArrInit item (map:Map<string, List<float>>) = Map.add item [0.0] map
let initAllArrVar (set:Set<string>) = Set.foldBack (fun item map -> varArrInit item map) set Map.empty

// Initialize the mapping of all boolean variables in given set to false
let varBInit item (map:Map<string,bool>) = map.Add(item, false)
let initAllBVar (set:Set<string> ) = Set.foldBack (fun item map ->  varBInit item map) set Map.empty
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
                    let mes = sprintf "ERROR: Unknown arithmetic variable %s in expression." x
                    failwith mes 
    | IndexExpr(A,x) -> 
            try 
                let vl = int (evalA x mapA arr)
                List.item vl (Map.find A arr)
            with err ->
                    let mes = sprintf "ERROR: Invalid lookup of index %d in array %s" (int (evalA x mapA arr)) A
                    failwith mes

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
    | Greater(x,y) -> (evalA x mapA arr)>(evalA y mapA arr)
    | GreaterEqual(x,y) -> (evalA x mapA arr)>=(evalA y mapA arr)
    | Less(x,y) -> (evalA x mapA arr)<(evalA y mapA arr)
    | LessEqual(x,y) -> (evalA x mapA arr)<=(evalA y mapA arr)
    | StrB(str) -> 
            try 
                Map.find str mapB
            with err -> 
                    let mes = sprintf "ERROR: Unknown boolean variable %s in expression." str
                    failwith mes
                        
// Evaluation of commands
let rec evalC e mapB (mapA:Map<string,float>) arr =
    match e with
    | Assign(str, vlu) -> (mapB, mapA.Add(str, evalA vlu mapA arr), arr)
    | ArrayAssign(str,index,vlu) -> 
                        let indx = int (evalA index mapA arr)
                        let vlue = (evalA vlu mapA arr)
                        let strArr = List.updateAt indx vlue (Map.find str arr)
                        (mapB, mapA, arr.Add(str, strArr))
    | _ -> (mapB, mapA, arr)

// Print sequence of values in arrays
let rec printSeq expr =
    match expr with
    | Singl(x) -> printA x
    | Seq(x, rest) -> (printA x) + ";"+ (printSeq rest)

// Print the parsed user input 
let rec printInp expr = 
    match expr with 
    | SetArith(str, ari) -> "SetArith(" + str + "=" + (printA ari) + ")"
    | SetBool(str,bol) -> "SetBool(" + str + (printB bol) + ")"
    | SetArray(str, list) -> "SetArray(" + str + "=" + (printSeq list)+"))"
    | SetDelim(inp1, inp2) -> "SetDelim(" + (printInp inp1) + (printInp inp2) + ")"

// Function to take the sequence of values and transform into list of evaluated expressions
let rec inpList expr mapA arr =
    match expr with 
    | Singl(x) -> [evalA x mapA arr]
    | Seq(x,next) -> evalA x mapA arr::inpList next mapA arr

// Functions to handle the input from the user in the memory mappings
let rec inputAMemory expr (mapA:Map<string,float>) arr = 
    match expr with
    | SetArith(str, ari) -> (mapA.Add(str, evalA ari mapA arr), arr)
    | SetArray(str, list) -> (mapA,arr.Add(str, inpList list mapA arr))
    | SetDelim(inp1, inp2) -> 
                        let (mapA1, arr1) = inputAMemory inp1 mapA arr
                        (inputAMemory inp2 mapA1 arr1)
    | SetBool(_) -> (mapA, arr)

let rec inputBMemory expr (mapB:Map<string,bool>) mapA arr = 
    match expr with 
    | SetBool(str, bol) -> mapB.Add(str,evalB bol mapB mapA arr)
    | SetDelim(inp1, inp2) ->  
                        let mapB1 = inputBMemory inp1 mapB mapA arr
                        inputBMemory inp2 mapB1 mapA arr
    | _ -> mapB

// Function to execute program statements based on edge list 
let rec executeGraph edgeList memory node steps = 
    match steps with
    | 0 -> "#INSUFFICIENT Program has run out of executing steps" 
    | _ ->
        let (mapB, mapA, arr) = memory 
        try 
            let edge = List.find (fun edg -> 
                match edg with
                | Ebool(ndx,bol,_) -> 
                        (evalB bol mapB mapA arr) && ndx=node
                | Ecomm(ndx,_,_) -> ndx=node) edgeList
            match edge with
            | Ecomm(node, com, next) -> 
                        let memory1 = evalC com mapB mapA arr 
                        let (sym,syf) = convert node next
                        let message = sprintf "Action: assignment\n Node q%s\n Memory->%A\n\n" sym memory1
                        if (next = (-1)) then
                                            let messageN = sprintf "Action: assignment\n Node q%s\n Memory->%A\n\n" syf memory1
                                            let termMes = sprintf "#TERMINATED Program has reached final node with %d steps left." (steps-1)
                                            message + messageN + termMes
                        else message + (executeGraph edgeList memory1 next (steps-1))
            | Ebool(node, bol, next) ->
                        let (sym,syf) = convert node next
                        let message = sprintf "Action: boolean test\n Node q%s\n Memory-> %A \n\n" sym memory
                        if (next = (-1)) then
                                            let messageN = sprintf "Action: assignment\n Node q%s\n Memory->%A\n\n" syf memory 
                                            message + messageN + "#TERMINATED Program has reached final node." 
                        else message + (executeGraph edgeList memory next (steps-1))
        with err -> 
                let (sym,_) = convert node 0
                let mes = sprintf "#STUCK No further edge can be taken. Program is stuck in node q%s with %d steps left.\n %A" sym steps memory     
                err.Message + "\n" + mes
