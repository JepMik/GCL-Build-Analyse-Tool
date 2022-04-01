module Verification
//#load "TypesAST.fs"
open TypesAST
//#load "ProgramGraph.fs"
open ProgramGraph
//#load "Interpreter.fs"
open Interpreter

// Checks that the edge is entering a certain node
let entryEdge init edge =
    match edge with
    | Ecomm(q,_,qf) when qf=init -> true
    | Ebool(q,_,qf) when qf=init -> true
    | _ -> false

// Procedure used in computing the set of short path fragments
let rec build init action final domP edgeList spf =
    let entries = List.filter (entryEdge init) edgeList
    List.fold (helper action final domP edgeList) spf entries
and helper action final domP edgeList spf edge =
    match edge with
    | Ecomm(q, alp, _ ) ->
        let lst = (C(alp))::action
        if Set.contains q domP
            then Set.add (q,lst,final) spf
            else build q lst final domP edgeList spf
    | Ebool(q, alp, _ ) -> 
        let lst = (B(alp))::action
        if Set.contains q domP
            then Set.add (q,lst,final) spf
            else build q lst final domP edgeList spf

// Algorithm that builds the set of short path fragments covering a program's graph
let rec buildSPF domP edgeList =
    Set.fold (fun spf node -> build node [] node domP edgeList spf) Set.empty domP
// Output -> set of short path fragments

// Function to extract predicate assignments and provide proof obligations
let rec extractPO spf predMemory = 
    Set.map (
        fun (init, frag, final) -> 
            match init, final with
            | 0,0 -> 
                    let (begpr, map, _) = predMemory
                    match Map.tryFind 0 map with
                    | Some (pred) -> (Pand (begpr, pred), frag, Pand (begpr, pred))
                    | None -> (begpr, frag, begpr)
            | -1,-1 -> 
                    let (_, map, endpr) = predMemory
                    match Map.tryFind -1 map with
                    | Some (pred) -> (Pand (pred, endpr), frag, Pand (pred, endpr))
                    | None -> (endpr, frag, endpr)
            | 0, -1 ->
                    let (begpr, _ , endpr) = predMemory
                    (begpr, frag, endpr)
            | x, y ->
                    let (_,map,_) = predMemory
                    match (Map.tryFind x map, Map.tryFind y map) with
                    | (Some(a), Some(b)) -> (a, frag, b)
                    | (Some(a), None) -> (a,frag, Pbool true)
                    | (None,Some(b)) -> (Pbool true,frag,b)
                    | (None,None) -> (Pbool true, frag,Pbool true)

        ) spf

// Convert a boolean expression to predicate
let rec boolToPred bol = 
    match bol with 
    | Bool(b) -> Pbool(b)
    | StrB(s) -> StrP(s)
    | ShortCircuitAnd(x,y) -> Pand(boolToPred x,boolToPred y)
    | ShortCircuitOr(x,y) -> Por(boolToPred x,boolToPred y)
    | LogAnd(x,y) -> Pand(boolToPred x,boolToPred y)
    | LogOr(x,y) -> Por(boolToPred x,boolToPred y)
    | Neg(x) -> Pnot(boolToPred x)
    | Equal(x,y) -> Pequal(x,y)
    | NotEqual(x,y) -> Pnequal(x,y)
    | Greater(x,y) -> Pgreater(x,y)
    | GreaterEqual(x,y) -> PgreaterEqual(x,y)
    | Less(x,y) -> Pless(x,y)
    | LessEqual(x,y) -> PlessEqual(x,y)


let rec substituteP pred id value =
    match pred with 
    | Pbool(x) -> pred
    | StrP(x) -> pred
    | Pand(x,y) -> Pand(substituteP x id value, substituteP y id value)
    | Por(x,y) -> Por(substituteP x id value, substituteP y id value)
    | Pnot(x) -> Pnot(substituteP x id value)
    | Pimply(x,y) -> Pimply(substituteP x id value, substituteP y id value)
    | Pequal(x,y) -> Pequal(substituteA x id value, substituteA y id value)
    | Pnequal(x,y) -> Pnequal(substituteA x id value, substituteA y id value)
    | Pgreater(x,y) -> Pgreater(substituteA x id value, substituteA y id value)
    | PgreaterEqual(x,y) -> PgreaterEqual(substituteA x id value, substituteA y id value)
    | Pless(x,y) -> Pless(substituteA x id value, substituteA y id value)
    | PlessEqual(x,y) -> PlessEqual(substituteA x id value, substituteA y id value)
and substituteA arith id value = 
    match arith with
    | Num(x) -> arith
    | StrA(x) when x=id -> value  
    | TimesExpr(x,y) -> TimesExpr(substituteA x id value, substituteA y id value)
    | DivExpr(x,y) -> DivExpr(substituteA x id value, substituteA y id value) 
    | PlusExpr(x,y) -> PlusExpr(substituteA x id value, substituteA y id value) 
    | MinusExpr(x,y) -> MinusExpr(substituteA x id value, substituteA y id value) 
    | PowExpr(x,y) -> PowExpr(substituteA x id value, substituteA y id value) 
    | UPlusExpr(x) -> UPlusExpr(substituteA x id value)
    | UMinusExpr(x) -> UMinusExpr(substituteA x id value)
    | LogExpr(x) -> LogExpr(substituteA x id value)
    | LnExpr(x) -> LnExpr(substituteA x id value)
    | IndexExpr(a,x) -> IndexExpr(a,substituteA x id value)
    | _ -> arith


// Helper function to go bottom-up in the proof tree (proof system)
let bottomUp postPred action = 
    match action with 
    | (B x) -> Pand (postPred, boolToPred x)
    | (C x) ->
        match x with
        | Assign(id,value) -> substituteP postPred id value
        // | ArrayAssign(id,index,val) -> substituteP
        | Skip -> postPred
        | _ -> postPred


// Function to construct the verification conditions throughout
let constrVC poset = 
    Set.map ( 
        fun (init, frag, final) -> 
            (init, List.fold bottomUp final frag)
    ) poset



// Pretty printer for predicates
let rec printPred pred = 
    match pred with 
    | Pbool(x) -> x.ToString()
    | StrP(x) -> x
    | Pand(x,y) -> "("+(printPred x)+")∧("+(printPred y)+")"
    | Por(x,y) -> "("+(printPred x)+")∨("+(printPred y)+")"
    | Pnot(x) -> "¬("+(printPred x)+")"
    | Pequal(x,y) -> (printA x)+"="+(printA y)
    | Pnequal(x,y) -> (printA x)+"!="+(printA y)
    | Pgreater(x,y) -> (printA x)+">"+(printA y)
    | PgreaterEqual(x,y) -> (printA x)+">="+(printA y)
    | Pless(x,y) -> (printA x)+"<"+(printA y)
    | PlessEqual(x,y) -> (printA x)+"<="+(printA y)
    | Pimply(x,y) -> "("+(printPred x)+")=>("+(printPred y)+")"


// Function to print short path fragment from list of actions
let rec printFragment lst = 
    match lst with
    | (B x)::tail -> (printB x)+"  "+(printFragment tail)
    | (C x)::tail -> (printC x 0)+"  "+(printFragment tail)
    | [] -> " "

// Function to print the set of short path fragments
let rec printSPF spf = 
    Set.fold (fun sp (p,lst,q) -> 
                    let (a,c) = convert p q
                    let str' = sprintf "q%s  %sq%s\n" a (printFragment lst) c
                    sp+str' ) "" spf

// Function to print the proof obligations
let rec printPO po = 
    Set.fold (fun str (pred, frag, post) -> 
                    let str' = sprintf "[%s]  %s[%s]\n" (printPred pred) (printFragment frag) (printPred post)
                    str+str' ) "" po


// Function to print the verification conditions
let rec printVC vc = 
    Set.fold (fun str (pred, comp) -> 
                    let str' = sprintf "%s => %s\n" (printPred pred) (printPred comp)
                    str+str' ) "" vc