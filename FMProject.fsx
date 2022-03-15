// This script implements our interactive calculator
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

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec evalA e =
  match e with
    | Num(x) -> x:float
    | TimesExpr(x,y) -> evalA(x) * evalA(y)
    | DivExpr(x,y) -> evalA(x) / evalA (y)
    | PlusExpr(x,y) -> evalA(x) + evalA (y)
    | MinusExpr(x,y) -> evalA(x) - evalA (y)
    | PowExpr(x,y) -> evalA(x) ** evalA (y)
    | UPlusExpr(x) -> evalA(x)
    | UMinusExpr(x) -> - evalA(x)
    | LogExpr(x) -> Math.Log(evalA(x),2)
    | LnExpr(x) -> Math.Log(evalA(x))
    | _ -> 0.0 //#TODO


// Function that parses a given input
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// Function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printf "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        //printfn "Result of evaluation: %f" (eval(e))
        //let str = (printA e)
        //printfn "%s" str;
        compute n
        with err -> compute (n-1)
//compute 3

let prettify ()=
    try
        printfn "Insert your Guarded Commands program to be parsed:"
        //Read console input
        let input = Console.ReadLine()
        //Create the lexical buffer
        let lexbuf = LexBuffer<char>.FromString input
        
        //Parsed result
        let res = FMProjectParser.start FMProjectLexer.tokenize lexbuf 
        printfn "<---Pretty print:--->"
        printfn "%s" (printC res 0)


    with e -> printfn "ERROR: %s" e.Message;;
//prettify();;

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
        
        let graphstr = "strict digraph {\n"+
                        let (list,x) = detGenenC res 0 -1 1
                        listGraph list
                        + "}\n"
        Console.WriteLine(graphstr)
        File.WriteAllText("graph.dot",graphstr)

        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected char: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;

let nondeter ()=
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
        
        let graphstr = "strict digraph {\n"+
                        let (list,x) = genenC res 0 -1 1
                        listGraph list
                        + "}\n"
        Console.WriteLine(graphstr)
        File.WriteAllText("graph.dot",graphstr)

        //Undefined string encountered   
        with e -> printfn "Parse error at : Line %i, %i, Unexpected char: %s" (lexbuf.EndPos.pos_lnum+ 1) 
                        (lexbuf.EndPos.pos_cnum - lexbuf.EndPos.pos_bol) (LexBuffer<_>.LexemeString lexbuf)

    with e -> printfn "ERROR: %s" e.Message;;


// Start interacting with the user

//printfn "%s" (printC (If(IfThen(Bool(true), Assign("x",Num(2))))))
//printfn "%s" (printB (LogAnd(Bool(false),Neg(Bool(true)))))
//let str = printC (parse (Console.ReadLine()))
//let str = (printC (parse (File.ReadAllText("test.txt") )))
//printfn "%s" str


let printMenu() = 
    printfn "Menu: "
    printfn "1. Pretty printer"
    printfn "2. Non-Deterministic Program Graph"
    printfn "3. Deterministic Program Graph"
    printfn "4. Exit menu"
    printf "Enter your choice: "

let getInput () = Int32.TryParse(Console.ReadLine())

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

menu()