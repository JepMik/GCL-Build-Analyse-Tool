omodule FMVerification
// #load "FMProjectTypesAST.fs"
open FMProjectTypesAST
// #load "FMProgramGraph.fs"
open FMProgramGraph
// #load "FMInterpreter.fs"
open FMInterpreter

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
        let str = (printC alp 0) + "; " + action
        if Set.contains q domP
            then Set.add (q,str,final) spf
            else build q str final domP edgeList spf
    | Ebool(q, alp, _ ) -> 
        let str = (printB alp) + "; " + action
        if Set.contains q domP
            then Set.add (q,str,final) spf
            else build q str final domP edgeList spf

// Algorithm that builds the set of short path fragments covering a program's graph
let rec buildSPF domP edgeList =
    Set.fold (fun spf node -> build node "" node domP edgeList spf) Set.empty domP

// Function to print the set of short path fragments
let rec printSPF spf = 
    Set.fold (fun sp (p,str,q) -> 
                    let (a,c) = convert p q
                    let str' = sprintf "q%s %s q%s \n" a str c
                    sp+str' ) "" spf

let rec printPO spf = 
    Set.fold (fun po (p,str,q) -> 
                    let (a,c) = convert p q
                    let str' = sprintf "[P(q%s)] %s [P(q%s)] \n" a str c
                    po+str' ) "" spf