//VERSION: 5.2


// This script implements our interactive GCL compiler & interpreter
// We need to import a couple of modules, including the generated lexer and parser

#r "FsLexYacc.Runtime.dll"

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
#load "./FM Secure/SecureParser.fs"
open SecureParser
#load "./FM Secure/SecureLexer.fs"
open SecureLexer
#load "SecurityAnalysis.fs"
open SecurityAnalysis
#load "ModelChecking.fs"
open ModelChecking

// Function that parses a given input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = ProgramParser.start ProgramLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res


let getInput () = Int32.TryParse(Console.ReadLine())

// Get input either from file or console
let rec chooseInput(file) = 
    printfn "Choose input source: "
    printfn "1. Text file ('%s')" file
    printfn "2. Console"
    printf "Enter your choice: "
    match getInput() with
    | true, 1 -> printfn "Input taken from file '%s'\n" file
                 File.ReadAllText(file)
    | true, 2 -> Console.ReadLine()
    | _ -> chooseInput(file)

// Apply the prettifier
let prettify () = 
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
    printfn "4. Program Sign Analysis"
    printfn "5. Model Checking"
    printfn "6. Return to main menu"
    printf "Enter your choice: "

// Allocate and initialize the interpreter memory with values
let memoryAlloc(edges, typ) =
    match typ with
    | "auto" -> 
                let (setB, setA, setArr) = varAllFinder edges
                let arithMap = initAllAVar setA 
                let arrayMap = initAllArrVar setArr
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

// Main function of the tool
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
                printfn "Insert sign memory for your Guarded Commands program
                        analysis (automatic signs for all missing variables):"
                //Read sign input
                let input = "ChickenNuggets! " + chooseInput("./FilesIN/InitialSigns.txt")
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
                            let mes = (sprintf "Parse error in sign assignment at : Line %i, %i, Unexpected token: %s" (lexbufInp.EndPos.pos_lnum+ 1) 
                                (lexbufInp.EndPos.pos_cnum - lexbufInp.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbufInp))
                            failwith mes
                    
                    #if DEBUG 
                    printfn "%A" resSign
                    #endif
                    // Automatic sign initialization
                    let (setB, setA, setArr) = varAllFinder edgeList
                    let (mapV, mapAr) = initSigns setA setArr

                    let AMem0 = signMemory resSign mapV mapAr
                    
                    // Run analysis algorithm
                    let solution = solveAnalysis 0 nodef (Set.add AMem0 Set.empty) edgeList
                    let (analSol, w) = solution
                    File.WriteAllText("./FilesOUT/SignAnalysis.txt", (printAnalysis analSol))
                    printfn "Sign Analysis Solution is printed in the file 'SignAnalysis.txt'!\n"
                    Console.WriteLine("Analysis solution --> \n" + (printAnalysis analSol))
                with e -> printfn "%s" e.Message
    | true, 5 -> 
                let (boolMap, arithMap, arrayMap) = memoryAlloc( edgeList, "user")     
                printfn "Initial memory configuration is shown below:"
                printfn "Memory ==> %A %A %A\n" boolMap arithMap arrayMap

                let initialState = (0, (boolMap, arithMap, arrayMap))
                let output = transition edgeList (Set.singleton initialState) Set.empty
                match output with 
                | "" -> printfn "Model Checker for given inputs is successful!"
                | _  -> printfn "%s" output
                printfn "Model Checking Solution is printed in the file 'ModelChecking.txt'!\n"
                File.WriteAllText("./FilesOUT/ModelChecking.txt", output)

    | true, 6 -> ()
    | _ -> runProgram edgeList domainP predMemory nodef

// Deterministic program handling
let determ () =
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
            
            try
                runProgram edgeList domainP predMemory x
            with e -> printfn "%s" e.Message

        //Undefined string encountered   
        with e -> printfn "Parse error in program at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)
    
    with e -> printfn "ERROR: %s" e.Message;;

let latticeMenu () = 
    printfn "Select one of the predefined lattices: "
    printfn "1. Confidentiality Lattice (public, private)"
    printfn "2. Integrity Lattice (trusted, dubious)"
    printfn "3. Classical Lattice (low, high)"
    printfn "4. Isolation Lattice (clean, Facebook, Google, Microsoft)"
    printf "Enter your choice: "

// Assign predefined security lattice 
let autoLattice raw =
    let selectAuto () = 
        latticeMenu ()
        match getInput() with 
        | true, 1 -> 
            printfn "Confidentiality lattice selected!"
            confidentiality
        | true, 2 -> 
            printfn "Integrity lattice selected!"
            integrity
        | true, 3 -> 
            printfn "Classical lattice selected!"
            classical
        | true, 4 ->
            printfn "Isolation lattice selected!"
            isolation
        | _ -> 
            printfn "Error"
            Set.empty
    if raw = AUTOL then 
        printfn "Provided security lattice is empty.\n"
        selectAuto ()
    else 
        let result = buildLattice raw Set.empty
        // Check if built security lattice is a partially ordered set
        if checkPartOrdered result 
        then
            printfn "Provided security lattice is a valid partially ordered set.\n" 
            result
        else
            printfn "Provided security lattice is not a partially ordered set.\nSelect predefined lattice instead.\n"
            selectAuto ()

// Deterministic security analysis
let secur () =

    try 
        printfn "Insert Guarded Commands program to be
                converted to a deterministic program graph:"
        //Read console input
        let program = chooseInput("./FilesIN/Program.txt")
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString program

        try 
            //Parsed program string
            let (Annot(begpr, prog, endpr)) = ProgramParser.start ProgramLexer.tokenize lexbuf

            printfn "Insert desired security lattice (automatic if not provided):"
            // Read console input for lattice
            let lattice = "FailedAgile!ButCSMPower!" + chooseInput("./FilesIN/Lattice.txt")
            // Create the lexical buffer
            let lexbuf = LexBuffer<char>.FromString lattice

            try 
                // Parse the security lattice string/input
                let latticeRaw = Option.get( 
                        match SecureParser.start SecureLexer.tokenize lexbuf with
                        | LAT (x) -> Some x
                        | CLS (x) -> None)

                #if DEBUG
                printfn "%A" latticeRaw
                #endif
                
                // Setup the security lattice
                let lattice = autoLattice latticeRaw
                    
                printfn "Insert desired security classification (automatic if not provided):"
                // Read console input for classification
                let classif = "DefinitelyNotSecure!" + chooseInput("./FilesIN/Classification.txt")
                // Create the lexical buffer
                let lexbuf = LexBuffer<char>.FromString classif

                try
                    let classifRaw = Option.get( 
                        match SecureParser.start SecureLexer.tokenize lexbuf with
                        | CLS (x) -> Some x
                        | LAT (x) -> None)
                    
                    #if DEBUG 
                    printfn "%A" classifRaw
                    #endif
                    
                    // Assign automatic classification of variables
                    let (setA, setArr) = peekCommand prog
                    let setVariables = Set.union setA setArr
                    let (lvlx, lvly) = Set.minElement lattice
                    let preClassification = autoClassify setVariables (lvlx, lvly)

                    if classifRaw = AUTOC then
                        printfn "Provided security classification is empty.\n   Automatic classification will be applied."
                        printfn "All variables have been classified as %s" lvlx

                    // Setup the classification memory
                    let classification = Map.fold (
                                            fun acc key value -> Map.add key value acc
                                            ) preClassification (buildClassification classifRaw Map.empty) 
                    // Compute the actual flows in the program
                    let actualFlows = secC prog Set.empty
                    let allowedFlows = allowFlows (lattice, classification) actualFlows
                    let result = secureProgram prog allowedFlows actualFlows

                    #if DEBUG
                    printfn "%A" lattice
                    printfn "%A" classification
                    printfn "%A" actualFlows
                    printfn "%A" allowedFlows
                    #endif

                    let fileOutput3 = sprintf "Security lattice configuration:\n   %s \n\n" (printLattice lattice)
                    File.WriteAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput3)

                    let fileOutput2 = sprintf "Security classification memory:\n   %s \n\n" (printClassification classification)
                    File.AppendAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput2)

                    let fileOutput = sprintf "Set of actual information flows in the program:\n    %s \n\n" (printSetFlows actualFlows)
                    File.AppendAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput)

                    let fileOutput1 = sprintf "Set of allowed information flows in the program:\n    %s \n\n" (printSetFlows allowedFlows)
                    File.AppendAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput1)

                    if Set.isEmpty result 
                        then 
                            let fileOutput = "Program is secure! \n" 
                            File.AppendAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput)
                            printfn "Program is secure!"
                        else 
                            let fileOutput = sprintf "Program is not secure! \nViolations of information flow: %s \n" (printSetFlows result)
                            File.AppendAllText("./FilesOUT/SecurityAnalysis.txt", fileOutput)
                            printfn "Program is not secure! \nViolations of information flow: %s" (printSetFlows result)

                    
                // Undefined string encountered in classification
                with e -> printfn "Parse error in security classification at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                            (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)
            
            // Undefined string encountered in security lattice
            with e -> printfn "Parse error in security lattice at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

        //Undefined string encountered in program
        with e -> printfn "Parse error in program at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum + 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;



// Non-deterministic program handling
let nondeter () =
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
        with e -> printfn "Parse error in program at : Line %i, %i, Unexpected token: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;
    


let printMenu () = 
    printfn "Menu: "
    printfn "1. Pretty printer"
    printfn "2. Non-Deterministic Program Environments"
    printfn "3. Deterministic Program Environments"
    printfn "4. Security Analysis"
    printfn "5. Exit menu"
    printf "Enter your choice: "

let rec menu () =     
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
    | true, 4 -> 
                secur()
                menu()
    | true, 5 -> ()
    | _ -> menu()
    

// Start interacting with the user
menu()
