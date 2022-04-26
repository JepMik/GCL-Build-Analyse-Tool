module SecurityAnalysis

open Interpreter
// BIG BRAIN PLAN:
// --> Fix input Parser/Lexer to handle security assignments
// --> Security lattice definition
// --> 

let fvArith expr = 
        let (setV, setA) = peekArith expr
        Set.union setV setA

let fvBool expr = 
        let (setB, setV, setA) = peekBool expr
        Set.union set B (Set.union setV setA)

