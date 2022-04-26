module ProgramGraph

open System
open System.IO
open TypesAST

// Function to compute done of a guarded command according to definition
let rec doneGC egc = 
    match egc with
    | IfThen(b,_) -> Neg(b)
    | FatBar(gc1,gc2) -> ShortCircuitOr(doneGC gc1, doneGC gc2)

// Compiler that takes GCL AST and converts to list of edges consisting 
// of (node(int), expression(command), node(int))

// Map union function
let newMap map1 map2 = Map.fold (fun acc key value -> Map.add key value acc) map1 map2

//Non-deterministic graphs generator
let rec genenC e init final next = 
    match e with
    | Order(c1,c2) ->let (E1,last,domP,predMap) = (genenC c1 init next (next+1)) 
                     let (E2,last2,domP2,predMap2) = (genenC c2 next final last)
                     (E1 @ E2, last2, Set.union domP domP2, newMap predMap predMap2) 
    | If(gc) -> let (E, last, domP,predMap) = genenGC gc init final next
                (E, last, domP, predMap)
    | Do(pred, gc) -> let (E,last,domP,predMap) = (genenGC gc init init next) 
                      (E @ [Ebool(init,doneGC gc,final)], last, Set.add init domP, Map.add init pred predMap)

    | _ -> ([Ecomm(init,e,final)], next, Set.empty, Map.empty)
and genenGC e init final next =                   
    match e with 
    | IfThen(b,com) -> 
            let (E,last,domP,predMap) = (genenC com next final (next+1))
            ([Ebool(init,b,next)] @ E, last, domP, predMap)
    | FatBar(gc1,gc2) -> 
            let (E1,last1,domP, predMap) = genenGC gc1 init final next             
            let (E2,last2,domP, predMap2) = genenGC gc2 init final last1
            (E1 @ E2, last2,domP, newMap predMap predMap2)


//Deterministic graphs generator
let rec detGenenC e init final next=
    match e with
        | Order(c1,c2) -> let (E1,last,domP,predMap) = (detGenenC c1 init next (next+1)) 
                          let (E2,last2,domP2,predMap2) = (detGenenC c2 next final last)
                          (E1 @ E2, last2, Set.union domP domP2, newMap predMap predMap2) 
        | If(gc) -> let (E,next,d,domP,predMap) = (detGenenGC gc init final next (Bool(false)))
                    (E, next, domP, predMap) 
        | Do(pred, gc) -> let (E,next,d,domP,predMap) = (detGenenGC gc init init next (Bool(false)))
                          (E @ [Ebool(init,Neg(d),final)], next, Set.add init domP, Map.add init pred predMap)
        | _ -> ([Ecomm(init,e,final)], next, Set.empty, Map.empty)
and detGenenGC e init final next d =
    match e with 
    | IfThen(b,com) -> let (E,last,domP,predMap) = (detGenenC com next final (next+1))
                       ([Ebool(init,ShortCircuitAnd(b,Neg(d)),next)] @ E, last, (ShortCircuitOr(b,d)), domP, predMap)
    | FatBar(gc1,gc2) -> let (E1,last1,d1,domP, predMap) = detGenenGC gc1 init final next d
                         let (E2,last2,d2,domP, predMap2) = detGenenGC gc2 init final last1 d1
                         (E1 @ E2, last2, d2, domP, newMap predMap predMap2)

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
    | ShortCircuitAnd(x,y) -> "("+(printB x)+")&&("+(printB y)+")"
    | ShortCircuitOr(x,y) -> "("+(printB x)+")||("+(printB y)+")"
    | LogAnd(x,y) -> "("+(printB x)+")&("+(printB y)+")"
    | LogOr(x,y) -> "("+(printB x)+")|("+(printB y)+")"
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
    | Do(_, x) -> "do "+(printGC x (n+1))+"\n"+"od"

// Conversion of node number to characters
let convert x y =
    match (x,y) with
    | (0,-1) ->("▷", "◀")
    | (0,0) -> ("▷","▷")
    | (0,d) -> ("▷",d.ToString())
    | (d,-1) -> (d.ToString(),"◀")
    | (d,0) -> (d.ToString(),"▷")
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


