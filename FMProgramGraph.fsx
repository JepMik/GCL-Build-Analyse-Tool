open System
open System.IO

module FMCompiler

#load "FMProjectTypesAST.fs"
open FMProjectTypesAST

//Function that computes depth and returns depth in int

 //   | Order(x,y) -> (printC x)+";"+Environment.NewLine+(printC y)
 //   | If(x) -> "if "+(printGC x)+Environment.NewLine+"fi"
 //   | Do(x) -> "do "+(printGC x)+Environment.NewLine+"od"
let rec depthC expr i =
    match expr with
    | Order(C1,C2) -> depthC C2 (i+(depthC C1 1))
    | If(GC) -> depthGC GC i 
    | _ -> i;
and depthGC expr i=
    | IfThen(b,C) -> depthC C (i+1)
    | FatBar(GC1, GC2) -> depthC 

//Compiler that takes GCL AST and convert that into syntax for a PG.
let graphC comL n = 
    match comL with 
    | Skip::tail -> ("q%d -- q%d [label=\"skip\"];" n (n+1)) + graphC tail (n+1) 
    | e::tail -> match e with
                 | Assign(x,y)::tail -> 


let genenC e list n dp =
    match e with
    | Skip -> Skip::list
    | Assign(x,y) -> 
    | ArrayAssign(x,y,z) -> 
//function generate of a function e
//(printC (If(IfThen(Bool(true), Assign("x",Num(2))))))


//Function that takes in list, and generate txt output in the correct dot format
//File.WriteAllText (filename , string output)  