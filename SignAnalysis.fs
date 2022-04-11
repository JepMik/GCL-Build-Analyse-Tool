module SignAnalysis

//#load "TypesAST.fs"
open TypesAST
//#load "AbstractOperators.fs"
open AbstractOperators

// Sign of a number, PIKA(+), NARUTO(-), ZORO(0)
let signN x = 
    match x with
    | n when n>0 -> PIKA
    | n when n<0 -> NARUTO
    | _ -> ZORO

// Converts nested sequence of signs to list of signs
let rec signList expr = 
    match expr with 
    | SSingl(x) -> [x]
    | SSeq(x,next) -> x::signList next

// Automatic sign initialization of found variables
let rec initSigns varSet arrSet = 
    (Set.fold (
        fun mem var ->
            Map.add var PIKA mem
    ) Map.empty varSet, Set.fold(
        fun mem arr -> 
            Map.add arr (Set.singleton PIKA) mem
    ) Map.empty arrSet)

// Creates the initial memory based on parsed sign memory
let rec signMemory expr mapV mapA = 
    match expr with
    | SignVariable(idf, sign) -> (Map.add idf sign mapV, mapA)
    | SignArray(idf, signs) -> (mapV,Map.add idf (Set.ofList(signList signs)) mapA)
    | SignDelim(x, y) -> 
                        let (mapV1, mapA1) = signMemory x mapV mapA
                        (signMemory y mapV1 mapA1)
//let aMem = signMemory (SignDelim(SignVariable("x",ZORO),SignArray("A",SSeq(ZORO,SSingl(PIKA))))) Map.empty Map.empty;;


// Arithmetic analysis function
let rec analArith arithAct memory = 
    match arithAct with
    | Num(x) -> Set.add (signN x) Set.empty
    | StrA(x) -> 
                let (mapV, mapA) = memory
                try 
                    Set.add (Map.find x mapV) Set.empty
                with err -> 
                    let mes = sprintf "ERROR: Unknown arithmetic variable %s in analysis." x
                    failwith mes
    | IndexExpr(A,x) ->
                let (mapV, mapA) = memory
                try 
                    if not (Set.isEmpty (Set.intersect (analArith x memory) (Set.empty.Add(ZORO).Add(PIKA))))
                        then (Map.find A mapA)
                        else Set.empty
                with err -> 
                    let mes = sprintf "ERROR: Unknown arithmetic array %s in analysis." A
                    failwith mes
    | TimesExpr(x,y) -> absTimes (analArith x memory) (analArith y memory)
    | DivExpr(x,y) -> absDiv (analArith x memory) (analArith y memory)
    | PlusExpr(x,y) -> absPlus (analArith x memory) (analArith y memory)
    | MinusExpr(x,y) -> absMinus (analArith x memory) (analArith y memory)
    | PowExpr(x,y) -> absPow (analArith x memory) (analArith y memory)
    | UPlusExpr(x) -> absUPlus (analArith x memory) 
    | UMinusExpr(x) -> absUMinus (analArith x memory) 
    | _ -> Set.empty

// Boolean analysis function
let rec analBool bolAct memory = 
    match bolAct with 
    | Bool(x) -> Set.add x Set.empty
    | ShortCircuitAnd(b1,b2) -> absSAND (analBool b1 memory) (analBool b2 memory)
    | ShortCircuitOr(b1,b2) -> absSOR (analBool b1 memory) (analBool b2 memory)
    | LogAnd(b1,b2) -> absAND (analBool b1 memory) (analBool b2 memory)
    | LogOr(b1,b2) -> absOR (analBool b1 memory) (analBool b2 memory)
    | Neg(b) -> absNot (analBool b memory)
    | Equal(a1,a2) -> absEqual (analArith a1 memory) (analArith a2 memory)
    | NotEqual(a1, a2) -> absNotEqual (analArith a1 memory) (analArith a2 memory)
    | Greater(a1,a2) -> absGreater (analArith a1 memory) (analArith a2 memory)
    | GreaterEqual(a1,a2) -> absGreaterEqual (analArith a1 memory) (analArith a2 memory)
    | Less(a1,a2) -> absLess (analArith a1 memory) (analArith a2 memory)
    | LessEqual(a1,a2) -> absLessEqual (analArith a1 memory) (analArith a2 memory)
    | _ -> Set.empty 




//--> Proof that the tool has a bug on FM4Fun and our solution is indeed correct

// let mex1= (Map [("j", PIKA)],Map [("A",(Set.singleton PIKA))])
// let mex2= (Map [("j", ZORO)],Map [("A",(Set.singleton PIKA))])
// let mex3= (Map [("j", NARUTO)],Map [("A",(Set.singleton PIKA))])

// analBool (Neg
//      (LogAnd
//         (Greater (StrA "j", Num 0),
//          Greater
//            (IndexExpr ("A", MinusExpr (StrA "j", Num 1)),
//             IndexExpr ("A", StrA "j"))))) mex3 //should contain true

// analBool (LogAnd
//         (Greater (StrA "j", Num 0),
//          Greater
//            (IndexExpr ("A", MinusExpr (StrA "j", Num 1)),
//             IndexExpr ("A", StrA "j")))) mex3 //should contain false

// analBool (Greater (StrA "j", Num 0),
//          Greater
//            (IndexExpr ("A", MinusExpr (StrA "j", Num 1)),
//             IndexExpr ("A", StrA "j"))) mex1
    




// powerMem > Mem 

// Iterates through the output of the arithmetic analysis 
// function and updates memory with signs - #small powerMemory
let updateAllSignsMem setSigns memory x = 
    Set.fold (
        fun mem sign ->
            let (mapV:Map<string, sign>, mapA) = memory
            Set.add (mapV.Add(x, sign),mapA) mem
        ) Set.empty setSigns

// let mexxx = (Map [("x", PIKA)],Map [("A",(Set.singleton PIKA))])
// updateAllSignsMem (analArith (MinusExpr(StrA("x"), Num 1)) mexxx) mexxx "x"
let updateAllMemAllSigns powerMem x a= 
    Set.fold (
                    fun pwrmem memory ->
                        let setSigns = analArith a memory
                        let newPMem = updateAllSignsMem setSigns memory x
                        Set.union newPMem pwrmem
                ) Set.empty powerMem

// Same for ArrayAssign
let updateSignMemAllInitSigns A memory sign = 
    let (mapV, mapA) = memory
    let sigmaA = Map.find A mapA
    Set.fold (
        fun pmem sp ->
            let setelim = Set.difference sigmaA (Set.add sp Set.empty)
            let setlow = Set.add sign setelim
            let sethigh = Set.add sign sigmaA
            
            Set.add (mapV,Map.add A setlow mapA) (Set.singleton (mapV,Map.add A sethigh mapA))
    ) Set.empty sigmaA

//updateSignMemAllInitSigns "A" (Map [("x", PIKA)],Map [("A",(Set.singleton PIKA))]) NARUTO
let updateAllSignsMemAllInitSigns A memory signSet = 
    Set.fold (
        fun pwrmem spp ->
            let pwrMemSign = updateSignMemAllInitSigns A memory spp
            Set.union pwrmem pwrMemSign
    ) Set.empty signSet
//updateAllSignsMemAllInitSigns "A" (Map [("x", PIKA)],Map [("A",(Set.singleton PIKA))]) (Set.add ZORO (Set.singleton NARUTO))

let updateEverything M A a1 a2 = 
    Set.fold (
        fun pwrmem memory ->
            let resInd = analArith a1 memory
            match (Set.intersect resInd (Set.empty.Add(PIKA).Add(ZORO)))=Set.empty with
            | false -> 
                        let signsSet = analArith a2 memory
                        let newPMem = updateAllSignsMemAllInitSigns A memory signsSet
                        Set.union newPMem pwrmem
            | true ->
                    // Negative index a1 found
                    Set.add memory pwrmem
    ) Set.empty M


// Specification analysis function
let rec analSpec action (M:Set<Map<string,sign> * Map<string, Set<sign>> >) =
    match action with
    | B (bol) -> 
            Set.fold (
                fun set memory ->
                    if Set.contains true (analBool bol memory) 
                        then Set.add memory set
                        else set
            ) Set.empty M
    | C (com) ->
            match com with
            | Assign(idf, value) -> updateAllMemAllSigns M idf value
            | ArrayAssign(idf, index, value) -> updateEverything M idf index value
            | _ -> M 

// analSpec (B(LogAnd (Less (StrA "i", StrA "n"), Neg (Bool false)))) (Set.singleton (Map [("i", PIKA);("n",PIKA)],Map [("A",(Set.singleton PIKA))]))
// analBool (LogAnd (Less (StrA "i", StrA "n"), Neg (Bool false))) (Map [("i", PIKA);("n",PIKA)],Map [("A",(Set.singleton PIKA))])

// Initialize analysis for all nodes
let initAnal first last mem0 = 
    let mutable analysis = Map.add first mem0 Map.empty
    for k in first+1..last do
        analysis <- Map.add k Set.empty analysis
    analysis <- Map.add (-1) Set.empty analysis
    #if DEBUG
    printfn "%A" analysis
    #endif
    analysis

// Recursive algorithms for analysis solution
let rec homeWork work edgeList analysis= 
    match (List.isEmpty work) with
    | false -> 
            let node = List.head work
            let rWork = List.tail work
            let edges = List.filter (
                            fun edge ->
                                match edge with
                                | Ebool(ni,_,_) -> ni=node
                                | Ecomm(ni,_,_) -> ni=node
                            ) edgeList
            let (upAnalysis, upWork) = List.fold (
                fun (anals,w) edge ->
                    #if DEBUG
                    printfn "%A" edge
                    #endif
                    match edge with 
                    | Ecomm(ni,com,nf) ->
                        if not (Set.isSubset (analSpec (C (com)) (Map.find ni anals)) (Map.find nf anals))
                            then (Map.add nf (Set.union (Map.find nf anals) (analSpec (C (com)) (Map.find ni anals))) anals, w@[nf])
                            else (anals, w)
                    | Ebool(ni,bol,nf) ->
                        if not (Set.isSubset (analSpec (B (bol)) (Map.find ni anals)) (Map.find nf anals))
                            then (Map.add nf (Set.union (Map.find nf anals) (analSpec (B (bol)) (Map.find ni anals))) anals, w@[nf])
                            else (anals, w)) (analysis,rWork) edges
            #if DEBUG
            printfn "New Step:\n %A %A" upAnalysis upWork
            #endif
            homeWork upWork edgeList upAnalysis
    | true -> (analysis, work)

// Worklist algorithm for analysis solution
let solveAnalysis first last mem0 edgeList =
    let analysis = initAnal first (last-1) mem0
    let work = List.singleton 0
    homeWork work edgeList analysis

let convNod x =
    match x with
    | 0 -> "▷" 
    | -1 -> "◀"
    | d -> d.ToString()

let printSign sign = 
    match sign with
    | PIKA -> "+"
    | ZORO -> "0"
    | NARUTO -> "-"

let printSignSet set = 
    Set.fold (fun str sign -> str+(printSign sign)+" ") "" set

// Print a memory
let printMemory memory = 
    let (mapV, mapA) = memory
    let strV = Map.fold(
                fun str var sign ->
                    let mes = sprintf " %s <=> %s |" (var) (printSign sign)
                    str+mes
                ) "" mapV
    let strA = Map.fold(
                fun str var signSet ->
                    let mes = sprintf " %s <=> %s |" (var) (printSignSet signSet)
                    str+mes
                ) "" mapA
    let final = sprintf "  Variables: %s \n  Arrays: %s\n\n" strV strA
    final
        
// Printer for the analysis solution
let printAnalysis solution = 
    Map.fold (
        fun str node powerMem ->
            let newMes = sprintf "Node --> q%s \n%s \n \n" (convNod node) (Set.fold (fun str mem -> str+" Sign Analysis -> \n"+(printMemory mem)) "" powerMem) 
            str+newMes
        ) "" solution

