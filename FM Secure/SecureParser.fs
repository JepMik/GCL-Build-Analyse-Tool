// Implementation file for parser generated by fsyacc
module SecureParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "SecureParser.fsp"

  open TypesAST

# 10 "SecureParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | FLOW
  | CLASIF
  | DELIM
  | TKNL
  | TKNC
  | VARIABLE of (string)
  | NUM of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_FLOW
    | TOKEN_CLASIF
    | TOKEN_DELIM
    | TOKEN_TKNL
    | TOKEN_TKNC
    | TOKEN_VARIABLE
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_cheatsec
    | NONTERM_seclattice
    | NONTERM_str
    | NONTERM_secclassif

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | FLOW  -> 1 
  | CLASIF  -> 2 
  | DELIM  -> 3 
  | TKNL  -> 4 
  | TKNC  -> 5 
  | VARIABLE _ -> 6 
  | NUM _ -> 7 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_FLOW 
  | 2 -> TOKEN_CLASIF 
  | 3 -> TOKEN_DELIM 
  | 4 -> TOKEN_TKNL 
  | 5 -> TOKEN_TKNC 
  | 6 -> TOKEN_VARIABLE 
  | 7 -> TOKEN_NUM 
  | 10 -> TOKEN_end_of_input
  | 8 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_cheatsec 
    | 3 -> NONTERM_cheatsec 
    | 4 -> NONTERM_cheatsec 
    | 5 -> NONTERM_cheatsec 
    | 6 -> NONTERM_seclattice 
    | 7 -> NONTERM_seclattice 
    | 8 -> NONTERM_str 
    | 9 -> NONTERM_secclassif 
    | 10 -> NONTERM_secclassif 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 10 
let _fsyacc_tagOfErrorTerminal = 8

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | FLOW  -> "FLOW" 
  | CLASIF  -> "CLASIF" 
  | DELIM  -> "DELIM" 
  | TKNL  -> "TKNL" 
  | TKNC  -> "TKNC" 
  | VARIABLE _ -> "VARIABLE" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | FLOW  -> (null : System.Object) 
  | CLASIF  -> (null : System.Object) 
  | DELIM  -> (null : System.Object) 
  | TKNL  -> (null : System.Object) 
  | TKNC  -> (null : System.Object) 
  | VARIABLE _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 2us; 65535us; 6us; 7us; 12us; 11us; 6us; 65535us; 4us; 14us; 6us; 8us; 9us; 10us; 12us; 8us; 15us; 16us; 18us; 14us; 2us; 65535us; 4us; 5us; 18us; 17us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 8us; 15us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 4us; 2us; 2us; 10us; 2us; 3us; 5us; 2us; 3us; 7us; 1us; 6us; 1us; 6us; 1us; 6us; 2us; 7us; 7us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 2us; 10us; 10us; 1us; 10us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 14us; 17us; 20us; 22us; 24us; 26us; 29us; 31us; 33us; 35us; 37us; 39us; 42us; |]
let _fsyacc_action_rows = 19
let _fsyacc_actionTableElements = [|2us; 32768us; 4us; 6us; 5us; 4us; 0us; 49152us; 1us; 32768us; 0us; 3us; 0us; 16385us; 1us; 16388us; 6us; 13us; 1us; 16386us; 3us; 18us; 1us; 16389us; 6us; 13us; 1us; 16387us; 3us; 12us; 1us; 32768us; 1us; 9us; 1us; 32768us; 6us; 13us; 0us; 16390us; 1us; 16391us; 3us; 12us; 1us; 32768us; 6us; 13us; 0us; 16392us; 1us; 32768us; 2us; 15us; 1us; 32768us; 6us; 13us; 0us; 16393us; 1us; 16394us; 3us; 18us; 1us; 32768us; 6us; 13us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 3us; 4us; 6us; 7us; 9us; 11us; 13us; 15us; 17us; 19us; 20us; 22us; 24us; 25us; 27us; 29us; 30us; 32us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 2us; 2us; 1us; 1us; 3us; 3us; 1us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 3us; 3us; 4us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 16392us; 65535us; 65535us; 16393us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 122 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : cheatSecure)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 131 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : cheatSecure)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "SecureParser.fsp"
                                                         _1
                   )
# 32 "SecureParser.fsp"
                 : cheatSecure));
# 142 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : classifVal)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "SecureParser.fsp"
                                                        CLS(_2) 
                   )
# 35 "SecureParser.fsp"
                 : cheatSecure));
# 153 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : latticeVal)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "SecureParser.fsp"
                                                        LAT(_2) 
                   )
# 36 "SecureParser.fsp"
                 : cheatSecure));
# 164 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "SecureParser.fsp"
                                                        CLS(AUTOC) 
                   )
# 37 "SecureParser.fsp"
                 : cheatSecure));
# 174 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "SecureParser.fsp"
                                                        LAT(AUTOL) 
                   )
# 38 "SecureParser.fsp"
                 : cheatSecure));
# 184 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'str)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'str)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "SecureParser.fsp"
                                                           LatFlow(_1,_3) 
                   )
# 51 "SecureParser.fsp"
                 : latticeVal));
# 196 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : latticeVal)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : latticeVal)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "SecureParser.fsp"
                                                           LatDelim(_1,_3) 
                   )
# 52 "SecureParser.fsp"
                 : latticeVal));
# 208 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "SecureParser.fsp"
                                                           _1 
                   )
# 60 "SecureParser.fsp"
                 : 'str));
# 219 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'str)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'str)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "SecureParser.fsp"
                                                             ClassFlow(_1,_3) 
                   )
# 64 "SecureParser.fsp"
                 : classifVal));
# 231 "SecureParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : classifVal)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : classifVal)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "SecureParser.fsp"
                                                             ClassDelim(_1,_3) 
                   )
# 65 "SecureParser.fsp"
                 : classifVal));
|]
# 244 "SecureParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 11;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : cheatSecure =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
