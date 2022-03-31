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

//let rec extractPO 

// Pretty printer for predicates
let rec printPred pred = 
    match pred with 
    | Pbool(x) -> x.ToString()
    | StrP(x) -> x
    | Pand(x,y) -> "("+(printPred x)+")∧("+(printPred y)+")"
    | Por(x,y) -> "("+(printPred x)+")∨("+(printPred y)+")"
    | Pnot(x) -> "¬("+(printB x)+")"
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
                    let str' = sprintf "q%s %s q%s \n" a (printFragment lst) c
                    sp+str' ) "" spf

// Function to print the proof obligations
let rec printPO spf = 
    Set.fold (fun po (p,lst,q) -> 
                    let (a,c) = convert p q
                    let str' = sprintf "[P(q%s)] %s [P(q%s)] \n" a (printFragment lst) c
                    po+str' ) "" spf
