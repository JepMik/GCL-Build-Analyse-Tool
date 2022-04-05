module SignAnalysis
#load "TypesAST.fs"
open TypesAST
#load "AbstractOperators.fs"
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

    

// Specification analysis function
let rec analSpec edge (powerMem:Set<Map<string,sign> * Map<string, Set<sign>> >) =
    match edge with
    | Ebool( _, bol, _ ) -> 
            Set.fold (
                fun set memory ->
                    if Set.contains true (analBool bol memory) 
                        then Set.add memory set
                        else set
            ) Set.empty powerMem
    | Ecomm(_,com,_) ->
            match com with
            | Assign(idf, value) ->
                Set.fold (
                    fun pwrmem memory ->
                        let result = analArith value memory
                        let newMem = Set.fold (
                                        fun mem sign ->
                                            let (mapV:Map<string, sign>, mapA) = memory
                                            Set.add (mapV.Add(idf, sign),mapA) mem
                                        ) Set.empty result
                        Set.union newMem pwrmem
                ) Set.empty powerMem
            // | ArrayAssign(idf, index, value) ->
            //     Set.fold (
            //         fun pwrmem memory ->
            //             let indexS = analArith index memory
            //             let result = analArith value memory
            //             Set.fold()
            //     ) Set.empty powerMem
            | _ -> powerMem 
    





