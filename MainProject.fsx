//VERSION: 4.5


// This script implements our interactive GCL compiler & interpreter
// We need to import a couple of modules, including the generated lexer and parser

#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
//#r "~/fsharp/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
open System.IO
#load "TypesAST.fs"
open TypesAST
#load "./FM Program/ProgramParser.fs"
open ProgramParser
#load "./FM Program/ProgramLexer.fs"
open ProgramLexer
#load "ProgramGraph.fs"
open ProgramGraph
#load "./FM Input/InputParser.fs"
open InputParser
#load "./FM Input/InputLexer.fs"
open InputLexer
#load "Interpreter.fs"
open Interpreter
#load "Verification.fs"
open Verification
#load "AbstractOperators.fs"
open AbstractOperators
#load "SignAnalysis.fs" 
open SignAnalysis

// Function that parses a given input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = ProgramParser.start ProgramLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


let getInput () = Int32.TryParse(Console.ReadLine())

let rec chooseInput(file) = 
    printfn "Choose input source: "
    printfn "1. Text file ('%s')" file
    printfn "2. Console"
    printf "Enter your choice: "
    match getInput() with
    | true, 1 -> printfn "Input taken from file '%s'" file
                 File.ReadAllText(file)
    | true, 2 -> Console.ReadLine()
    | _ -> chooseInput(file)


let prettify ()=
    try
        printfn "Insert your Guarded Commands program to be parsed:"
        //Read console input
        let input = chooseInput("./FilesIN/Program.txt")
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString input
        
        try
            //Parsed result
            let result = ProgramParser.start ProgramLexer.tokenize lexbuf 
            let (Annot(begpr, prog, endpr)) = result
            printfn "<----Pretty print:---->"
            printfn "%s" (printC prog 0)
            printfn "Prettified program is also available in 'OUTPretty.txt'!"
            File.WriteAllLines("./FilesOUT/Pretty.txt",[(printC prog 0)])

        with e -> printfn "Parse error at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                    (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)
    with err ->printfn "%s" err.Message

//prettify();;



let printInnerMenu () = 
    printf "Options:\n"
    printfn "1. Step-wise Execution with Automatic Input"
    printfn "2. Step-wise Execution with User Input"
    printfn "3. Program Verification"
    printfn "4. Program Analysis"
    printfn "5. Return to main menu"
    printf "Enter your choice: "


let memoryAlloc(edges, typ) =
    match typ with
    | "auto" -> 
                let (setB', setA') = varBFinder edges
                let (setB, setA) = (setB', Set.union setA' (varAFinder edges))
                let arithMap = initAllAVar setA
                let arrayMap = Map.empty
                let boolMap = initAllBVar setB
                printfn "Automatic variable initialization is applied"
                (boolMap,arithMap,arrayMap)
    | "user" ->
                printfn "Insert input values for your Guarded Commands program:"
                //Read console input
                let input = "ChuggyChug! "+chooseInput("./FilesIN/Input.txt")
                //Create the lexical buffer
                let lexbufInp = LexBuffer<char>.FromString input
                try 
                    // Parsed input result
                    let resInpRaw = InputParser.start InputLexer.tokenize lexbufInp
                    let resInp = Option.get( 
                        match resInpRaw with
                        | I (x) -> Some x
                        | S (x) -> None)
                    let (arithMap, arrayMap) = inputAMemory resInp Map.empty Map.empty
                    let boolMap = inputBMemory resInp Map.empty arithMap arrayMap
                    printfn "User variable initialization is applied"
                    (boolMap,arithMap,arrayMap)
                with e -> 
                    let mes = (sprintf "Parse error at : Line %i, %i, Unexpected token: %s" (lexbufInp.EndPos.pos_lnum+ 1) 
                        (lexbufInp.EndPos.pos_cnum - lexbufInp.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbufInp))
                    failwith mes
                    (Map.empty, Map.empty, Map.empty)


    | _ -> (Map.empty, Map.empty, Map.empty)

let rec runProgram edgeList domainP predMemory nodef = 
        
    printInnerMenu()
    match getInput() with
    | true, 1 ->  
                let (boolMap, arithMap, arrayMap) = memoryAlloc( edgeList, "auto")            
                printfn "Initial memory assigned is shown below:"
                printfn "Memory ==> %A %A %A" boolMap arithMap arrayMap

                printfn "Input maximal number of steps"
                let (x,steps) = getInput()
                let execStr = executeGraph edgeList (boolMap, arithMap, arrayMap) 0 steps
                File.WriteAllText("./FilesOUT/StepExecution.txt",execStr)
                printfn "Check step-wise execution logs in 'StepExecution.txt'!"

    | true, 2 -> 
                let (boolMap, arithMap, arrayMap) = memoryAlloc( edgeList, "user")     
                printfn "Initial memory assigned is shown below:"
                printfn "Memory ==> %A %A %A" boolMap arithMap arrayMap
                
                printfn "Input maximal number of steps"
                let (x,steps) = getInput()
                let execStr = executeGraph edgeList (boolMap, arithMap, arrayMap) 0 steps
                File.WriteAllText("./FilesOUT/StepExecution.txt",execStr)
                printfn "Check step-wise execution logs in 'StepExecution.txt'!"

    | true, 3 ->
                let SPF = buildSPF domainP edgeList
                File.WriteAllText("./FilesOUT/ShortPathFragments.txt", printSPF SPF)
                printfn "Short Path Fragments are printed in the file 'ShortPathFragments.txt'!"
                Console.WriteLine("SPF: --> \n"+printSPF SPF)

                let PO = extractPO SPF predMemory
                File.WriteAllText("./FilesOUT/ProofObligations.txt", printPO PO)
                printfn "Proof Obligations are printed in the file 'ProofObligations.txt'!"
                Console.WriteLine("Proof Obligations: --> \n"+printPO PO)

                let VC = constrVC PO
                File.WriteAllText("./FilesOUT/VerificationConditions.txt", printVC VC)
                printfn "Verification Conditions are printed in the file 'VerificationConditions.txt'!"
                Console.WriteLine("Verification: --> \n"+printVC VC)
    | true, 4 -> 
                printfn "Insert sign memory for your Guarded Commands program analysis:"
                //Read sign input
                let input = "ChickenNuggets! "+Console.ReadLine()
                //Create the lexical buffer
                let lexbufInp = LexBuffer<char>.FromString input
                try 
                    // Parsed sign memory
                    let resSign =
                        try
                            let resSignRaw = InputParser.start InputLexer.tokenize lexbufInp
                            let resSign = Option.get( 
                                match resSignRaw with
                                | S (x) -> Some x
                                | I (x) -> None)
                            resSign
                        with e -> 
                            let mes = (sprintf "Parse error at : Line %i, %i, Unexpected token: %s" (lexbufInp.EndPos.pos_lnum+ 1) 
                                (lexbufInp.EndPos.pos_cnum - lexbufInp.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbufInp))
                            failwith mes
                    
                    #if DEBUG 
                    printfn "%A" resSign
                    #endif

                    let AMem0 = signMemory resSign Map.empty Map.empty
                    
                    // Run analysis algorithm
                    
                    let solution = solveAnalysis 0 nodef (Set.add AMem0 Set.empty) edgeList
                    let (analSol, w) = solution
                    File.WriteAllText("./FilesOUT/SignAnalysis.txt", (printAnalysis analSol))
                    printfn "Sign Analysis Solution is printed in the file 'SignAnalysis.txt'!"
                    Console.WriteLine("Analysis solution --> \n" + (printAnalysis analSol))
                

                with e -> printfn "%s" e.Message

    | true, 5 -> ()
    | _ -> runProgram edgeList domainP predMemory nodef


let determ ()=
    try
        printfn "Insert your Guarded Commands program to be
                converted to a deterministic program graph:"
        //Read console input
        let input = chooseInput("./FilesIN/Program.txt")
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString input
        try 

            //Parsed result
            let (Annot(begpr, prog, endpr)) = ProgramParser.start ProgramLexer.tokenize lexbuf  
            let (edgeList, x, domP, predMap) = detGenenC prog 0 -1 1  
            let domainP = Set.add (-1) (Set.add 0 domP)
            let predMemory = (begpr, predMap, endpr)

            let graphstr = "strict digraph {\n"+
                            listGraph edgeList
                            + "}\n"

            File.WriteAllText("./FilesOUT/Graph.dot",graphstr)
            printfn "The GCL program graph is printed in file 'Graph.dot' and can be visualized!"

            runProgram edgeList domainP predMemory x

        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)
    
    with e -> printfn "ERROR: %s" e.Message;;


let nondeter()=
    try
        printfn "Insert your Guarded Commands program to be 
                converted to a non-deterministic program graph:"
        //Read console input
        let program = chooseInput("./FilesIN/Program.txt")
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString program
        try 
            //Parsed result
            let (Annot(begpr, prog, endpr)) = ProgramParser.start ProgramLexer.tokenize lexbuf 
            let (edgeList,x,domP,predMap) = genenC prog 0 -1 1
            let domainP = Set.add (-1) (Set.add 0 domP)
            let predMemory = (begpr, predMap, endpr)

            let graphstr = "strict digraph {\n"+
                            listGraph edgeList
                            + "}\n"
            
            printfn "The GCL program graph is printed in file 'Graph.dot' and can be visualized!"
            File.WriteAllText("./FilesOUT/Graph.dot",graphstr)

            try 
                runProgram edgeList domainP predMemory x
            with e -> printfn "%s" e.Message

        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;


let printMenu () = 
    printfn "Menu: "
    printfn "1. Pretty printer"
    printfn "2. Non-Deterministic Program Environments"
    printfn "3. Deterministic Program Environments"
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
menu()