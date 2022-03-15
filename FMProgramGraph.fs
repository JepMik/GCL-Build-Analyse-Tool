module FMProgramGraph

open System
open System.IO

// This file implements a module where we define multiple data types
// to store represent boolean, arithmetic, commands and guarded commands from Parser and Lexer.

open FMProjectTypesAST

//Function to compute done of a guarded command
let rec doneGC egc = 
    match egc with
    | IfThen(b,_) -> Neg(b)
    | FatBar(gc1,gc2) -> LogOr(doneGC gc1, doneGC gc2)

//Compiler that takes GCL AST and converts to list of edges consisting of (node(int), expression(command), node(int))

//Non-deterministic graphs generator
let rec genenC e init final next = 
    match e with
    | Order(c1,c2) ->let (E1,last) = (genenC c1 init next (next+1)) 
                     let (E2,last2) = (genenC c2 next final last)
                     (E1 @ E2, last2) 
    | If(gc) -> (genenGC gc init final next)
    | Do(gc) -> let (E,last) = (genenGC gc init init next) 
                (E @ [Ebool(init,doneGC gc,final)], last)
    | _ -> ([Ecomm(init,e,final)], next)
and genenGC e init final next =                   
    match e with 
    | IfThen(b,C) -> 
            let (E,last) = (genenC C next final (next+1))
            ([Ebool(init,b,next)] @ E, last)
    | FatBar(gc1,gc2) -> 
            let (E1,last1) = genenGC gc1 init final next             
            let (E2,last2) = genenGC gc2 init final last1
            (E1 @ E2, last2)


//Deterministic graphs generator
let rec detGenenC e init final next=
    match e with
        | Order(c1,c2) -> let (E1,last) = (detGenenC c1 init next (next+1)) 
                          let (E2,last2) = (detGenenC c2 next final last)
                          (E1 @ E2, last2) 
        | If(gc) -> let (E,next,d) = (detGenenGC gc init final next (Bool(false)))
                    (E, next) 
        | Do(gc) -> let (E,next,d) = (detGenenGC gc init init next (Bool(false)))
                    (E @ [Ebool(init,Neg(d),final)], next)
        | _ -> ([Ecomm(init,e,final)], next)
and detGenenGC e init final next d =
    match e with 
    | IfThen(b,C) -> let (E,last) = (detGenenC C next final (next+1))
                     ([Ebool(init,LogAnd(b,Neg(d)),next)] @ E, last, (LogOr(b,d)))
    | FatBar(gc1,gc2) -> let (E1,last1,d1) = detGenenGC gc1 init final next d
                         let (E2,last2,d2) = detGenenGC gc2 init final last1 d1
                         (E1 @ E2, last2, d2)

// "Pretty Printer" for arithmetic expressions to show precedence of the operators
let rec printA e =
    match e with
    | StrA(x) -> x
    | Num(x) -> x.ToString()
    | TimesExpr(x,y) -> "("+(printA x)+"*"+(printA y)+")"
    | DivExpr(x,y) -> "("+(printA x)+"/"+(printA y)+")"
    | PlusExpr(x,y) -> "("+(printA x)+"+"+(printA y)+")"
    | MinusExpr(x,y) -> "("+(printA x)+"-"+(printA y)+")"
    | PowExpr(x,y) -> "("+(printA x)+"^"+(printA y)+")"
    | UPlusExpr(x) -> "(+"+(printA x)+")"
    | UMinusExpr(x) -> "(-"+(printA x)+")"
    | IndexExpr(A,x) -> A+"["+(printA x)+"]"
    | LogExpr(x) -> "log("+(printA x)+")"
    | LnExpr(x) -> " ln("+(printA x)+")"

// "Pretty Printer" for boolean expressions to show precedence of the operators
let rec printB e = 
    match e with 
    | Bool(x) -> x.ToString()
    | StrB(x) -> x
    | ShortCircuitAnd(x,y) -> "("+(printB x)+")&("+(printB y)+")"
    | ShortCircuitOr(x,y) -> "("+(printB x)+")|("+(printB y)+")"
    | LogAnd(x,y) -> "("+(printB x)+")&&("+(printB y)+")"
    | LogOr(x,y) -> "("+(printB x)+")||("+(printB y)+")"
    | Neg(x) -> "!("+(printB x)+")"
    | Equal(x,y) -> (printA x)+"="+(printA y)
    | NotEqual(x,y) -> (printA x)+"!="+(printA y)
    | Greater(x,y) -> (printA x)+">"+(printA y)
    | GreaterEqual(x,y) -> (printA x)+">="+(printA y)
    | Less(x,y) -> (printA x)+"<"+(printA y)
    | LessEqual(x,y) -> (printA x)+"<="+(printA y)

// "Pretty Printer" for guarded commands and commands to show precedence of the operators
let rec indent n =
    match n with
    //| _ -> ""
    | 0 -> ""
    | x -> "   "+(indent (x-1))

let rec printGC e n = 
    match e with
    | IfThen(x,y) -> (printB x)+" -> \n"+(printC y n)
    | FatBar(x,y) -> (printGC x n)+"\n"+"[] "+(printGC y n)
and printC e n=
    match e with
    | ArrayAssign(x,y,z) -> (indent n)+x+"["+(printA y)+"]:="+(printA z)
    | Assign(x,y) -> (indent n)+x+":="+(printA y)
    | Skip -> (indent n)+"skip"
    | Order(x,y) -> (indent n)+(printC x n)+";\n"+(indent n)+(printC y n)
    | If(x) -> "if "+(printGC x (n+1))+"\n"+"fi"
    | Do(x) -> "do "+(printGC x (n+1))+"\n"+"od"

let convert x y =
    match (x,y) with
    | (0,-1) ->("►", "◄")
    | (0,d) -> ("►",d.ToString())
    | (d,-1) -> (d.ToString(),"◄")
    | (d,0) -> (d.ToString(),"►")
    | (d1,d2) -> (d1.ToString(),d2.ToString())

//Function that takes in list and generates graphviz syntax
let rec listGraph edgeL= 
    match edgeL with 
    | Ebool(x,b,y)::tail -> 
                let (a,c) = convert x y
                "q"+a+" -> q"+c+"[label=\""+(printB b)+"\"];\n"  
                                + (listGraph tail)
    | Ecomm(x,com,y)::tail -> 
                let (a,c) = convert x y
                "q"+a+" -> q"+c+"[label=\""+(printC com 0)+"\"];\n"  
                                + (listGraph tail)
    | [] -> ""
let graphstr = "strict digraph {\n"+
                listGraph [Ecomm (1, Assign ("x", Num 2), 2); Ebool (0, Bool true, 1)]
                + "}\n" // Example
//File.WriteAllText (filename , string output)  
File.WriteAllText("graph.dot",graphstr);;

