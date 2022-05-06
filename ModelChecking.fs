module ModelChecking

open Interpreter
open ProgramGraph
open Interpreter

// Printer function for a state
let printState (node, memory) =
    let (nodeS, x) = convert node 0
    sprintf "Node: q%s <=> Memory->\n%s" nodeS (printMemory memory)

// Algorithm for computing reachable states in one 
let Reach1 edgeList state = 
    let (node, (mapB, mapA, arr)) = state
    let edges1 = 
        List.filter (
            fun edg -> 
                match edg with
                | Ebool(ndx,bol,_) -> ndx=node
                | Ecomm(ndx,_,_) -> ndx=node) edgeList
    List.fold (fun (set, str) edge ->
                match edge with
                | Ebool (ndx,bol,nfx) ->
                        try 
                            let test = evalB bol mapB mapA arr
                            if test then 
                                (Set.add (nfx, (mapB, mapA, arr)) set, str)
                            else (set, str)
                        with e ->
                            (set, str + sprintf "STUCK %s\n%s\n\n" (printState state) e.Message)
                | Ecomm (ndx,com,nfx) -> 
                        try 
                            let memory = evalC com mapB mapA arr
                            (Set.add (nfx, memory) set, "")
                        with e ->
                            (set,str + sprintf "STUCK %s\n%s\n\n" (printState state) e.Message)

                            
                ) (Set.empty, "") edges1

// Algorithm for Model Checking
let rec transition edgeList toExplore visited =
    match (Set.isEmpty toExplore) with 
    | false -> 
        let state = Set.minElement toExplore
        let newExp = Set.remove state toExplore
        match Set.contains state visited with
        | true -> sprintf "%s" (transition edgeList newExp visited)
        | false -> 
            let newVis = Set.add state visited
            #if DEBUG
            printfn "%A %A %A" toExplore visited state
            #endif

            let (nextStates, stuckString) = Reach1 edgeList state
            if Set.isEmpty nextStates 
                then 
                    let (node, memory) = state
                    sprintf "%s%s" stuckString (transition edgeList newExp newVis)
                else 
                    let updExp = Set.fold (
                                    fun set state -> Set.add state set
                                    ) newExp nextStates
                    sprintf "%s" (transition edgeList updExp newVis)  
    | true -> sprintf ""
