# 🅖🅒🅛 🅟🅐🅡🅢🅔🅡                                 

![Up to Date](https://github.com/ikatyang/emoji-cheat-sheet/workflows/Up%20to%20Date/badge.svg)
<img src="https://img.shields.io/badge/Version-6.3-red"/>
<img src="https://img.shields.io/badge/Powered%20By-CheckMate-%23ffc933"/>
# **This parser for Guarded Commands Language was created by:**

*s196671 Josefine Rosalie Balch Petersen*

*s204683 Adrian Zvizdenco*

*s204708 Jeppe Moeller Mikkelsen*

## **How to Run the Code**
**It is absolutely necessary to have fsyacc and fslexx installed to run the GCL-parser**
[If not installed follow the instructions](https://gitlab.gbar.dtu.dk/02141/mandatory-assignment/blob/master/getting-started-fs.md)

### **Input to run code**
*Inputs to terminal:* 
1. dotnet fsi MainProject.fsx
2. Menu of options appear

When running the code a menu should pop-up with the options specified below.
The input files potentially used are located in the `FilesIN` folder and can be edited there, while the output files are going to be updated in the `FilesOUT` folder.
### **Menu Options:**
    1. Pretty Printer 
    2. Non-Deterministic Program Environments
    3. Deterministic Program Environments
    4. Exit Menu
After choosing an option, the input should be of Dijkstra's Guarded Command Language (possibly with annotations). The tool provides 2 methods to input 
the program - one from a txt file (Input.txt or Program.txt) and one from the console:

    Choose input source:
    1. Text file ('___.txt')
    2. Console

If the input is not a valid GCL, the parser will acknowledge and send the user a special error message and tell where the error has been found, as in the following example. 

    Parse error at : Line 1, 6, Unexpected token: :=

#### **Extended Menu Options:**
When choosing either a Non-Deterministic Program Graph or a Deterministic Program Graph, the program graph will be constructed. Afterwards the following extended menu will be given:

    1. Step-wise Execution with Automatic Input
    2. Step-wise Execution with User Input
    3. Program Verification
    4. Return to main menu
This extended menu gives the user the option of seeing a step-wise execution with either automated-input or with user-input for variables and arrays, perform a program verification or simply return to the main menu as given above.


## **Syntax of our Code**
**These commands represent an extended set of Dijkstra's Guarded Commands Language syntactics**
| Expressions | Description |
| --- | --- | 
| Arithmetics   a : := |  n \| x \| A\[a\] \| a + a \| a - a \| a * a \| a / a \| - a \| a ^ a \| (a) \| ln a \| log a 
| Boolean b : := | true \| false \| b & b \| b \| b \| b && b \| b \|\| b \| !b  \| a = a \| a != a \| a > a \| a >= a \| a < a \| a <= a \| (b) 
| Commands C : := | x := a \| A\[a\] := a \| skip \| C ; C \| if GC fi \| do GC od \| do \[P\] GC od 
| Guarded Commands GC : := | b -> C \| GC [] GC 
| Predicates P: := | true \| false \| x \| P ∨ P \| P ∧ P \| ¬P \| P => P \| a = a \| a != a \| a > a \| a >= a \| a < a \| a <= a \| (P)
| Annotations   AN: := | begin \[P\] C end \[P\] \| C end \[P\] \| begin \[P\] C \| C

[If commands not understood, a reference can be found here](https://en.wikipedia.org/wiki/Guarded_Command_Language#:~:text=In%20a%20guarded%20command%2C%20just,statement%20will%20not%20be%20executed.)

*Exceptions to be aware of:*

Skip should have two spaces after input  `|skip--|`, hyphens are in these case supposed to be spaces.

If one doesn't annotate the program, but chooses to perform a program verification, all predicates are going to be initialized to the value `true`.

After each of the annotating tags (begin | end) there is a whitespace to delimit the predicate in brackets.

### **Program Graphs:**
The Program Graphs are developed by two main functions:
1. Generate Deterministic Program Graphs
2. Generate Non-Deterministic Program Graphs

These function takes the expressions and turn them into a list of edges containing `(node(int), expression(command), node(int))`.

 These lists are of course different, depending on the choice of the desired Program Graph. The differences in syntax for the Non- and Deterministic Program Graphs can be seen in the book Formal Methods chap. 2 from *Course 02141 - Computer Science Modelling on DTU*.

In order to generate the Program Graphs, it is then processed by a function that translates the list of edges into the syntax for graphviz, and generate a file called `graph.dot`. This file contains the program graph (consider reading *How to interpret program graph results*).


## **Step-Wise Execution**
The step-wise execution shows how the memory changes throughout the execution of the code and its actions and which nodes each execution takes place.

From the Extended Menu Options, the user can choose to either input variables and arrays themselves or get automated input generated. Afterwards the user chooses how many execution steps they want shown. This will then initialize the Step-Wise Execution and generate a file called `StepExecution.txt`. This file contains the step-wise execution of the program, consider reading *How to interpret the Step-wise Execution*.


## **How to Interpret the Outcome of the Prettifier**
**The outcome:** 

Is a *"Pretty Printed"* AST, that shows how the arithmetic, boolean or other commands are being treated by the Parser and Lexer.

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
## **Error**
Parse errors are implemented in our GCL-parser, and should yield an error message, if the Lexer recognizes strings that are not defined.

If for some reason an error occurs while running an already defined syntax, then please contact the creators.

## **How to Interpret Program Graph Results:**
**The outcome:**

When running option `2. Non-Deterministic Program Graph` or `3. Deterministic Program Graph` from the Main Menu. A file named `graph.dot` will be generated, where the syntax of the [graphviz text language](https://graphviz.org/doc/info/lang.html) will be inside. The syntax will change each time the program is run, depending on the input of the Guarded Command Language.

If using VS-code, and [extension](https://marketplace.visualstudio.com/items?itemName=joaompinto.vscode-graphviz) has been downloaded. The graph can be seen directly in the program, utilizing the extension.

Otherwise [this link](https://edotor.net/). The syntax of the `graph.dot` can be dragged and dropped, and hereby shown in graph format.


## **How to Interpret the Step-Wise Execution:**
**The outcome:**

When running option `1. Step-wise Execution with Automatic Input` or `2. Step-wise Execution with User Input` from the Extended Menu Options. A file named `execution.txt` will be generated, where the syntax of step-wise execution is written in the following manner:

    Action: ____
        Node: ___
        Memory-> (map [], map [("variable", value)], map [])

`Action` denotes the action which has been done, could be assignment of a variable or a boolean check.

`Node` denotes the note where the the program ends up after the action has been done.

`Memory` denotes the memory with the new variables after the action. The first `map []` inside the memory denotes boolean variables, the second `map []` inside the memory denotes arithmetic variables, and the third `map []` inside the memory denotes arrays.

### **Terminated or Stuck**

The step-wise execution of the input Guarded Commands will run either until it is terminates, gets stuck or runs out of amount of steps. 
The amount of steps shown is user-input, thus the number inputted may not be sufficient enough for the step-wise execution to either terminate or get stuck. The program can then be run again with a larger amount of steps to determine whether or not the program terminates or gets stuck later on. 

Depending on whether the program terminates or gets stuck, one of the following messages will be shown.

If the program executes until it runs out of possible steps, the following is printed:

    #INSUFFICIENT Program has run out of executing steps
OR if it TERMINATES by achieving the final node, the following message will be shown:

    #TERMINATED Program has reached final node.


If the program gets STUCK with the given variables and within the set amount of steps, the following message will be given, providing information on where the program gets stuck, why it gets stuck and with how many steps left:

    #STUCK No further edge can be taken. Program is stuck in node __ with __ steps left.
with one of the possible errors (or one of the F# Compiler exceptions):

    ERROR: Unknown arithmetic variable __ in expression.
    ERROR: Unknown boolean variable __ in expression.
    ERROR: Invalid lookup of index __ in array __

## **How to Interpret the Program Verification**

According to the annotations provided in the program, the tool we built establishes the covering nodes of the program graph:

    1. Initial q▷ and final q◀ node
    2. A node for each loop contained in the graph.

Based on this domain of covering nodes, the short path fragments are computed and printed in their corresponding file `ShortPathFragments.txt`, as in the snippet below, based on an iteration computing the sum:

    q▷  x:=0  i:=1   q2
    q2  !(i<=n)   q◀
    q2  i<=n  x:=(x+i)   q2

Eventually, having predicate assignments for each of these nodes, the tool computes the Proof Obligations of the program verification and prints them in the file `ProofObligations.txt`:

    [(x=0)∧(n>0)]  x:=0  i:=1   [(i<n)∧(x>=i)]
    [(i<n)∧(x>=i)]  !(i<=n)   [i>=n]
    [(i<n)∧(x>=i)]  i<=n  x:=(x+i)   [(i<n)∧(x>=i)]

Along the proof tree of each fragment some inference rules are applied from the ending predicate `P(q◀)` (bottom-up). Thus, a predicate B to be implied from the start predicate is obtained and the verification condition `P(q▷) => B` is formed. All of them are printed in the file `VerificationConditions.txt`:

    (x=0)∧(n>0) => (1<n)∧(0>=1)
    (i<n)∧(x>=i) => ((i<n)∧((x+i)>=i))∧(i<=n)
    (i<n)∧(x>=i) => (i>=n)∧(¬(i<=n))

## **How to Interpret Sign Analysis Solution**
**Sign Analysis:**

Sign Analysis is an interpretation of a given program, where initial signs defining the values of the variables and array contents are evaluated dynamically based on programs nodes/edges. 

The sign analysis is not a concrete analyzing tool, since it utilizes abstract memories and operators such as: 



| ⨣ |      -    |   0   |     +     |
|---|:---------:|------:|----------:|
| - | {−}       |  {−}  | {−, 0, +} |
| 0 | {−}       |  {0}  |    {+}    |
| + | {−, 0, +} |  {+}  |    {+}    |

When running the Program Sign Analysis in the menu, the user will be asked to provide some initial sign information.
The signs available are the set `{-,0,+}`. The information for each field should be delimited by comma `','`.To enforce the sign for a variable use `a=+` and for an array use `A={+,-}`. If a variable or array is missing in declaration of signs, it would be automatically assigned to `+`.
 
**The outcome:** 

The output of the tool will be sent to the file `SignAnalysis.txt`. 
Here the user will be able to see the solutions for the final node(`q◀`) and initial node(`q▷`) (pay attention to the order in the file), and the other nodes (numbered) with their respective sign analysis assignment, in the following format: 

    Node --> q5 
    Sign Analysis -> 
    Variables:  i <=> + | j <=> 0 | n <=> + | t <=> + | x <=> - | 
    Arrays:  A <=> +  |

    Sign Analysis -> 
    Variables:  i <=> - | j <=> + | n <=> + | t <=> + | x <=> + | 
    Arrays:  A <=> + - 0 |

    Sign Analysis -> 
    Variables:  i <=> + | j <=> - | n <=> + | t <=> 0 | x <=> + | 
    Arrays:  A <=> +  |

## **How to Interpret Security Analysis**
**Security Analysis:**
Security Analysis is a form of verification of a program's information flows. Based on a security lattice and a classification of variables, selected by the user, the tool can compute if any violations of information flows are present in the program or if the program is secure in that aspect. This analyzing tool can only be applied on deterministic program graphs.

The security lattice can be defined by specifying information flows from one security level to another `public -> private, trusted -> dubious`, delimited by commas `,` . The security lattice is considered valid, if it is a partially ordered set and can be illustrated in a Hasse diagram. 

If no security lattice is provided, the user can choose from 4 predefined security lattices in the respective menu: 
1. Confidentiality: public ⊑ private
2. Integrity: trusted ⊑ dubious
3. Classical: low ⊑ high 
4. Isolation: clean ⊑ Facebook, clean ⊑ Google, clean ⊑ Microsoft

The security classification can be defined by providing a security level for each variable appearing in the program `x = public, y = private`, delimited by commas `,` . Any missing variable classifications will be assigned automatically.

The user has to provide all the information either from the console or from a specified file.

**The Outcome:**

The output of the analyzing tool is redirected to the file `SecurityAnalysis.txt`. 
In the first rows, the security lattice and security classification chosen are presented for context: 

    Security lattice configuration:
    classified ⊑ top_secret; private ⊑ classified; public ⊑ private;

    Security classification memory:
    A ∈ classified; i ∈ public; j ∈ private; n ∈ top_secret; t ∈ classified;  


Following, the user will be able to analyze the set of flows actually happening in the program and the set of flows allowed to happen in the program according to the lattice rules:

    Set of actual information flows in the program:
        A->A; A->j; A->t; i->A; i->i; i->j; i->t; j->A; j->j; j->t;  n->A; n->i; n->j; n->t; t->A;

    Set of allowed information flows in the program:
        A->A; A->t; i->i; i->j; j->A; j->j; j->t; t->A;  

Finally, based on the above sets, the tools finds all the violations of information flow in the program and outputs them. If the set of violations is empty, the program is considered `SECURE` and `INSECURE`, otherwise:

    Program is not secure! 
    Violations of information flow: A->j; i->A; i->t; n->A; n->i; n->j; n->t;  


*Project completed within DTU course 02141 - Computer Science Modelling*