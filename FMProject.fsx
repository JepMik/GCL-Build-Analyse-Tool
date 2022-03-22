// This script implements our interactive GCL compiler & interpreter
// We need to import a couple of modules, including the generated lexer and parser

#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
//#r "~/fsharp/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
open System.IO
#load "FMProjectTypesAST.fs"
open FMProjectTypesAST
#load "FMProjectParser.fs"
open FMProjectParser
#load "FMProjectLexer.fs"
open FMProjectLexer
#load "FMProgramGraph.fs"
open FMProgramGraph
// #load "FMInputAST.fs"
// open FMInputAST
#load "FMInputParser.fs"
open FMInputParser
#load "FMInputLexer.fs"
open FMInputLexer
#load "FMInterpreter.fs"
open FMInterpreter

// Function that parses a given input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


let getInput () = Int32.TryParse(Console.ReadLine())


let prettify ()=
    try
        printfn "Insert your Guarded Commands program to be parsed:"
        //Read console input
        let input = Console.ReadLine()
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString input
        
        //Parsed result
        let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf 
        printfn "<----Pretty print:---->"
        printfn "%s" (printC res 0)


    with e -> printfn "ERROR: %s" e.Message;;
//prettify();;



let printInnerMenu () = 
    printf "Options:\n "
    printfn "1. Step-wise Execution with Automatic Input"
    printfn "2. Step-wise Execution with User Input"
    printfn "3. Return to main menu"
    printf "Enter your choice: "


let memoryAlloc(edges, typ) =
    match typ with
    | "auto" -> 
                let arithMap = initAllAVar (varAFinder edges)
                let arrayMap = Map.empty
                let boolMap = initAllBVar (varBFinder edges)
                printfn "Automatic variable initialization is applied"
                (boolMap,arithMap,arrayMap)
    | "user" ->
                printfn "Insert input values for your Guarded Commands program:"
                //Read console input
                let input = Console.ReadLine()
                //Create the lexical buffer
                let lexbufInp = LexBuffer<char>.FromString input
                try 
                    // Parsed input result
                    let resInp = FMInputParser.start FMInputLexer.tokenize lexbufInp

                    let (arithMap, arrayMap) = inputAMemory resInp Map.empty Map.empty
                    let boolMap = inputBMemory resInp Map.empty arithMap arrayMap
                    printfn "User variable initialization is applied"
                    (boolMap,arithMap,arrayMap)
                with e -> 
                    (printfn "Parse error at : Line %i, %i, Unexpected char: %s" (lexbufInp.EndPos.pos_lnum+ 1) 
                    (lexbufInp.EndPos.pos_cnum - lexbufInp.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbufInp))
                    (Map.empty, Map.empty, Map.empty)
                


    | _ -> (Map.empty, Map.empty, Map.empty)

let rec executeSteps edges = 
        
    printInnerMenu()
    match getInput() with
    | true, 1 ->  
                let (boolMap, arithMap, arrayMap) = memoryAlloc( edges, "auto")            
                printfn "%A" boolMap
                printfn "%A" arithMap
                printfn "%A" arrayMap

                let (x,steps) = getInput()
                let execStr = executeGraph edges (boolMap, arithMap, arrayMap) 0 steps
                File.WriteAllText("execution.txt",execStr)
                printfn "Check step-wise execution logs in 'execution.txt'!"

    | true, 2 -> 
                let (boolMap, arithMap, arrayMap) = memoryAlloc( edges, "user")            
                printfn "%A" boolMap
                printfn "%A" arithMap
                printfn "%A" arrayMap

                printfn "Input maximal number of steps"
                let (x,steps) = getInput()
                let execStr = executeGraph edges (boolMap, arithMap, arrayMap) 0 steps
                File.WriteAllText("execution.txt",execStr)
                printfn "Check step-wise execution logs in 'execution.txt'!"
    | true, 3 -> ()
    | _ -> executeSteps (edges)


let determ ()=
    try
        printfn "Insert your Guarded Commands program to be
                converted to a deterministic program graph:"
        //Read console input
        let input = Console.ReadLine()
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString input
        try 

            //Parsed result
            let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf  
            let (edgeList,x) = detGenenC res 0 -1 1          
            let graphstr = "strict digraph {\n"+
                            listGraph edgeList
                            + "}\n"
            Console.WriteLine(graphstr)
            File.WriteAllText("graph.dot",graphstr)

            executeSteps edgeList


        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected char: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)
    
    with e -> printfn "ERROR: %s" e.Message;;


let nondeter()=
    try
        printfn "Insert your Guarded Commands program to be 
                converted to a deterministic program graph:"
        //Read console input
        let program = Console.ReadLine()
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString program
        try 
            //Parsed result
            let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf 
            let (edgeList,x) = genenC res 0 -1 1
            let graphstr = "strict digraph {\n"+
                            listGraph edgeList
                            + "}\n"
            Console.WriteLine(graphstr)
            File.WriteAllText("graph.dot",graphstr)

            executeSteps edgeList

        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected char: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;

let printMenu () = 
    printfn "Menu: "
    printfn "1. Pretty printer"
    printfn "2. Non-Deterministic Program Graph"
    printfn "3. Deterministic Program Graph"
    printfn "4. Exit menu"
    printf "Enter your choice: "

let rec menu() = 
    printMenu()
    match getInput() with
    | true, 1 -> 
                prettify()
                menu()
    | true, 2 ->
                nondeter()
                menu()
    | true, 3 -> 
                determ()
                menu()
    | true, 4 -> ()
    | _ -> menu()
    

// Start interacting with the user

//printfn "%s" (printC (If(IfThen(Bool(true), Assign("x",Num(2))))))
//printfn "%s" (printB (LogAnd(Bool(false),Neg(Bool(true)))))
//let str = printC (parse (Console.ReadLine()))
//let str = (printC (parse (File.ReadAllText("test.txt") )))
//printfn "%s" str





menu()