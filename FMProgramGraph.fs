module FMProgramGraph

open System
open System.IO

// This file implements a module where we define multiple data types
// to store represent boolean, arithmetic, commands and guarded commands from Parser and Lexer.

open FMProjectTypesAST

//Function that computes depth and returns depth in int
let rec depthC expr =
    match expr with
    | Order(C1,C2) -> (depthC C2) + (depthC C1)
    | If(GC) -> depthGC GC 
    | Do(GC) -> 1+depthGC GC
    | _ -> 1
and depthGC expr =
    match expr with
    | IfThen(_,C) -> 1 + depthC C
    | FatBar(gc1, gc2) -> depthGC gc1 + depthGC gc2;

//Function to compute done of a guarded command
let rec doneGC egc = 
    match egc with
    | IfThen(b,_) -> Neg(b)
    | FatBar(gc1,gc2) -> LogOr(doneGC gc1, doneGC gc2)

//Compiler that takes GCL AST and converts to list of edges consisting of (node(int), expression(command), node(int))
//Non-deterministic graphs generator
let rec genenC e ni nf =
    match e with
    | Order(c1,c2) -> (genenC c1 ni (ni+depthC c1)) 
                        @ (genenC c2 (ni+(depthC c1)) nf) 
    | If (gc) -> (genenGC gc ni nf 1) 
    | Do (gc) -> (genenGC gc ni ni 1) @ [Ebool(ni,doneGC gc,nf)]
    | _ -> [Ecomm(ni,e,nf)]
and genenGC e ni nf dp =
    match e with
    | IfThen(b,C) ->  (genenC C (ni+dp) nf) @ [Ebool(ni,b,ni+dp)]
    | FatBar(gc1,gc2) -> (genenGC gc1 ni nf 1) @ (genenGC gc2 ni nf (depthGC gc1))
    

//Deterministic graphs generator
let rec detGenenC e ni nf= 
    match e with
    | Order(c1,c2) -> (detGenenC c1 ni (ni+depthC c1)) 
                        @ (detGenenC c2 (ni+(depthC c1)) nf)
    | If (gc) -> let (E,d) = detGenenGC gc ni nf 1 (Bool(false))
                 E
    | Do (gc) -> let (E,d) = detGenenGC gc ni ni 1 (Bool(false)) 
                 E @ [Ebool(ni,Neg(d),nf)]
    | _ -> [Ecomm(ni,e,nf)]
and detGenenGC e ni nf dp d = 
    match e with 
    | IfThen(b, c) -> let E = genenC c (ni+dp) nf 
                      ([Ebool(ni,LogAnd(b,Neg(d)),ni+dp)]) @ E, (LogOr(b,d))
    | FatBar(gc1,gc2) -> let (E1,d1) = detGenenGC gc1 ni nf 1 d
                         let (E2,d2) = detGenenGC gc2 ni nf (depthGC gc1) d1
                         (E1 @ E2, d2)
//function generate of a function e
//((If(IfThen(Bool(true), Assign("x",Num(2))))))



// "Pretty Printer" for arithmetic expressions to show precedence of the operators
let rec printA e =
    match e with
    | StrA(x) -> x
    | Num(x) -> x.ToString()
    | TimesExpr(x,y) -> (printA x)+"*"+(printA y)
    | DivExpr(x,y) -> (printA x)+"/"+(printA y)
    | PlusExpr(x,y) -> (printA x)+"+"+(printA y)
    | MinusExpr(x,y) -> (printA x)+"-"+(printA y)
    | PowExpr(x,y) -> (printA x)+"^"+(printA y)
    | UPlusExpr(x) -> "+"+(printA x)
    | UMinusExpr(x) -> "-"+(printA x)
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
let rec printGC e = 
    match e with
    | IfThen(x,y) -> (printB x)+" -> "+(printC y)
    | FatBar(x,y) -> (printGC x)+"\n"+"[] "+(printGC y)
and printC e =
    match e with
    | ArrayAssign(x,y,z) -> x+"["+(printA y)+"]:="+(printA z)
    | Assign(x,y) -> x+":="+(printA y)
    | Skip -> "skip"
    | Order(x,y) -> (printC x)+";\n"+(printC y)
    | If(x) -> "if "+(printGC x)+"\n"+"fi"
    | Do(x) -> "do "+(printGC x)+"\n"+"od"
// accumulating recursive to be implemented
//Environment.NewLine changed to '\n'

//Function that takes in list and generates graphviz syntax
let rec listGraph edgeL= 
    match edgeL with 
    | Ebool(x,b,y)::tail -> "q"+x.ToString()+" -> q"+y.ToString()+"[label=\""+(printB b)+"\"];\n"  
                                + (listGraph tail)
    | Ecomm(x,com,y)::tail -> "q"+x.ToString()+" -> q"+y.ToString()+"[label=\""+(printC com)+"\"];\n"  
                                + (listGraph tail)
    | [] -> ""
let graphstr = "strict digraph {\n"+
                listGraph [Ecomm (1, Assign ("x", Num 2), 2); Ebool (0, Bool true, 1)]
                + "}\n" // Example
//File.WriteAllText (filename , string output)  
File.WriteAllText("graph.dot",graphstr);;