# 🅖🅒🅛 🅟🅐🅡🅢🅔🅡                                   

# This parser for Guarded Commands Language was created by:

*s196671 Josefine Rosalie "Queen" Balch Petersen*

*s204683 Adrian "Black Pawn" Zvizdenco*

*s204708 Jeppe "White Pawn" Moeller Mikkelsen*


## How to run the code
**It is absolutely neccessary to have fsyacc and fslexx installed to run the GCL-parser**
[If not installed follow the instructions](https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/getting-started-fs.md)

*Exceptions to be aware of:*

Skip should have two spaces after input  `|skip--|`, hyphens are in these case supposed to be spaces.

### Steps:
*Inputs to terminal:*
1. dotnet fsi FMProject.fsx
2. `Now input show be typed into the terminal, and the outcome should the "Pretty Printed" AST`
3. `Repeat:` dotnet fsi FmProject.fsx `to do new input`

## Syntax of our code
**These commands should be understood by people following the course Computer Science Modelling cn: 02141 on DTU**
| Commands | Description |
| --- | --- | 
| Arithmetics   a : : |  n \| x \| A[a] \| a + a \| a - a \| a * a \| a / a \| - a \| a ^ a \| (a) | ln a | log a
| Boolean b : : | true \| false \| b & b \| b \| b \| b && b \| b \|\| b \| !b  \| a = a \| a != a \| a > a \| a >= a \| a < a \| a <= a \(b) |
| Commands C : :| x := a \| A[a] := a \| skip \| C ; C \| if GC fi \| do GC od |
| Guarded Commands GC : :| b -> C \| GC [] GC |

[If commands not understood, a reference can be found here](https://en.wikipedia.org/wiki/Guarded_Command_Language#:~:text=In%20a%20guarded%20command%2C%20just,statement%20will%20not%20be%20executed.)



## How to interpret the outcome of the prettifier
**The outcome:** 
Is a *"Pretty Printed"* AST, that shows how the arithmetic, boolean or other commands
are being treated by the Parser and Lexer.

*Example:*
| Input | Outcome |
| --- | --- |
| `if true -> x:=2 fi` | IFFI ( TRUE -> x:= 2) |
| `if x>=y -> z:=x [] y>x -> z:=y fi` | IFFI ( GREATEREQUAL ( x,y ) -> z := x [] GREATER ( y,x ) -> z := y ) |
|  `do true -> skip  od` | DOOD ( TRUE -> SKIP)|
| `dax := ln 1` | dax := LN ( 1 ) |
|               |

These examples show a clear image of how the AST is formed by the combined work of the Parser and Lexer.
## Error
Parse errors are implemented in our GCL-parser, and should yield an error message, if the Lexer recognizes strings that are not defined.

If for some reason an error occurs while running an already defined syntax, then please contact the creators.
