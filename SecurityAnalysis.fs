module SecurityAnalysis

open Interpreter
// BIG BRAIN PLAN:
// --> Fix input Parser/Lexer to handle security assignments
// --> Security lattice definition
// --> 

// Free variables in arithmetic expressions
let fvArith expr = 
    let (setV, setA) = peekArith expr
    Set.union setV setA

// Free variables in boolean expressions
let fvBool expr = 
    let (setB, setV, setA) = peekBool expr
    Set.union setB (Set.union setV setA)

// Function to transform the parsed lattice into a set of flows
let rec buildLattice rawexpr set = 
    match rawexpr with
    | LatFlow(x,y) -> Set.add (x,y) set
    | LatDelim(x,y) -> buildLattice x (buildLattice y set)
    | _ -> set

// Function to transform the parsed classifications into a map classification
let rec buildClassification rawexpr map = 
    match rawexpr with 
    | ClassFlow(x,y) -> Map.add x y map
    | ClassDelim(x,y) -> buildClassification x (buildClassification y map)
    | _ -> map

let rec autoClassify setVar (lvlx, lvly) = 
    Set.fold (
        fun map var -> Map.add var lvlx map
    ) Map.empty setVar

// Function that takes two sets and builds a flow between sets (=>)
let doubleFlow setx sety = 
    Set.fold ( 
        fun set x -> 
            let setu = Set.fold(
                fun setv y -> 
                    Set.add (x,y) setv
                        ) Set.empty sety
            Set.union set setu
    ) Set.empty setx
//doubleFlow (Set.ofList [1;2;3]) (Set.ofList [4;5;6]);; // For testing the function

// Security semantic functions
let rec secC command setX = 
    match command with
    | Skip -> Set.empty
    | Assign(x,a) -> doubleFlow (Set.union (fvArith(a)) setX) (Set.singleton x)
    | ArrayAssign(A, a1, a2) -> doubleFlow (Set.union (Set.union (fvArith(a1)) (fvArith(a2)) ) setX) (Set.singleton A)
    | Do(_,gc) -> 
                let (w, d) = secGC gc (Bool(false), setX)
                w  
    | If(gc) -> 
                let (w,d) = secGC gc (Bool(false), setX)
                w
    | Order(c1,c2) ->
                Set.union (secC c1 setX) (secC c2 setX)
and secGC guardcom (d, setX) =
    match guardcom with
    | IfThen(b,com) -> 
                let w = secC com (Set.union (Set.union (fvBool(b)) setX) (fvBool(d)) )
                (w, ShortCircuitOr(b,d) )
    | FatBar(gc1,gc2) ->
                let (w1,d1) = secGC gc1 (d,setX)
                let (w2,d2) = secGC gc2 (d1,setX)
                (Set.union w1 w2, d2)

// Predefined lattices of two levels
let confidentiality = Set.singleton ("public", "private")
let integrity = Set.singleton ("trusted", "dubious")
let classical = Set.singleton ("low", "high")

// Permited (allowed) flows computed based on lattice, 
//classification and actual flows in program
let rec allowFlows (lattice, classif) actual = 
    Set.fold (
        fun set (x, y) ->
            let levelx = Map.find x classif
            let levely = Map.find y classif
            // THIS NOT WORKING!
            match Set.contains (levelx, levely) lattice with
            | true -> Set.add (x,y) set
            | false -> set
    ) Set.empty actual

let tryAllow  = allowFlows (confidentiality, Map.ofList [("x","public");("y","private")]) (Set.ofList [("x","y");("y","x")]);;

// Check if flows in program are a subset of the allowed flows 
let rec secureProgram program allowedFlows actualFlows =
    Set.difference actualFlows allowedFlows

//secureProgram (Assign(StrA("x"), StrA("y"))) tryAllow;;

