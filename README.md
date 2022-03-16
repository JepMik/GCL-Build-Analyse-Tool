# 🅖🅒🅛 🅟🅐🅡🅢🅔🅡                                   

# This parser for Guarded Commands Language was created by:

*s196671 Josefine Rosalie Balch Petersen*

*s204683 Adrian Zvizdenco*

*s204708 Jeppe Moeller Mikkelsen*

## How to run the code
**It is absolutely neccessary to have fsyacc and fslexx installed to run the GCL-parser**
[If not installed follow the instructions](https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/getting-started-fs.md)

### Input to run code
*Inputs to terminal:* 
1. dotnet fsi FMProject.fsx
2. Menu of options appear

When running the code a menu should pop-up with the options specified below.
### Menu Options:
    1. Pretty Printer 
    2. Non-Deterministic Program Graph
    3. Deterministic Program Graph
    4. Exit Menu
After chosing an option, the input should be of Dijkstra's Guarded Command Language.
If the input is not a valid GCL, the parser will acknowledge and send the user a special error message and tell where the error have been found. 




## Syntax of our code
**These commands represent an extended set of Dijkstra's Guarded Commands Language syntactics**
| Commands | Description |
| --- | --- | 
| Arithmetics   a : : |  n \| x \| A\[a\] \| a + a \| a - a \| a * a \| a / a \| - a \| a ^ a \| (a) \| ln a \| log a 
| Boolean b : : | true \| false \| b & b \| b \| b \| b && b \| b \|\| b \| !b  \| a = a \| a != a \| a > a \| a >= a \| a < a \| a <= a \| (b) 
| Commands C : :| x := a \| A\[a\] := a \| skip \| C ; C \| if GC fi \| do GC od 
| Guarded Commands GC : :| b -> C \| GC [] GC 

[If commands not understood, a reference can be found here](https://en.wikipedia.org/wiki/Guarded_Command_Language#:~:text=In%20a%20guarded%20command%2C%20just,statement%20will%20not%20be%20executed.)

*Exceptions to be aware of:*

Skip should have two spaces after input  `|skip--|`, hyphens are in these case supposed to be spaces.


### Program graphs:
The Program Graphs are developed by two main functions:
1. Generate Deterministic Program Graphs
2. Generate Non-Determinisitc Program Graphs

These function takes the expressions and turn them into a list of egdes containing `(node(int), expression(command), node(int))`.

 These lists are of course different, depending on the choice of the desired Program Graph. The differences in syntax for the Non- and Deterministic Program Graphs can be seen in book Formal Methods chap. 2 from *Course 02141 - Computer Science Modelling on DTU*.

The generation of the Program Graphs, are then processed by a function that translates the list of egdes into the syntax for graphviz, and generate a file called `graph.dot`. This file contains the program graph, consider reading *How to interpret program graph results*.


## How to interpret the outcome of the prettifier
**The outcome:** 

Is a *"Pretty Printed"* AST, that shows how the arithmetic, boolean or other commands
are being treated by the Parser and Lexer.

*Example:*
| Input | Outcome | Underlying AST |
| --- | --- | --- |
| `if true -> x:=2 fi` | `if True -> x:=2 fi` | IFFI ( TRUE -> x:= 2) |
| `if x>=y -> z:=x [] y>x -> z:=y fi` | `if x>=y -> z:=x [] y>x -> z:=y fi` | IFFI ( GREATEREQUAL ( x,y ) -> z := x [] GREATER ( y,x ) -> z := y ) |
|  `do true ->  skip  od` | `do True -> skip  od` | DOOD ( TRUE -> SKIP)|
| `dax := ln 1` | `dax := ln(1)` | dax := LN ( 1 ) |
| `x:=3*(4^2+5)` | `x := 3*((4^2)+5)` | x := TIMES( 3, ADD( POW(4, 2), 5))
|               |                      |

These examples show a clear image of how the AST is formed by the combined work of the Parser and Lexer.
## Error
Parse errors are implemented in our GCL-parser, and should yield an error message, if the Lexer recognizes strings that are not defined.

If for some reason an error occurs while running an already defined syntax, then please contact the creators.

## How to interpret program graph results:
**The outcome:**

When running option `2. Non-Deterministic Program Graph` or `3. Deterministic Program Graph`. A file named `graph.dot` file will be generated, where the syntax of the [graphziz text language](https://graphviz.org/doc/info/lang.html) will be inside. The syntax will change each time the program is run, depending on the input of the Guarded Command Language.

If using VS-code, and [extension](https://marketplace.visualstudio.com/items?itemName=joaompinto.vscode-graphviz) has been downloaded. The graph can be seen directly in the program, utilsing the extension.

Otherwise [this link](https://edotor.net/). The syntax of the `graph.dot` can be dragged and dropped, and hereby shown in graph format.


*Project completed within DTU course 02141 - Computer Science Modelling*
