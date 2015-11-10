# hoz
## A Semantic Interpreter for Oz Kernel Language

### Specifications
A program in Oz is just a statement. Any statement in Oz is represented as a list (in Oz). In general, the source code is given as an __Abstract Syntax Tree__ (AST), which is represented as a nested list.

We have implemented an interpreter which takes such an AST as input and outputs the effect of the program in terms of the memory allocations it does.

Oz consists of a __Single Assignment Store__ (SAS), like an abstract form of memory, where variables are allocated space and values of variables are stored. We have interpreted the given program by __Operational Semantics__, which is essentially an exhaustive enumeration of the steps which a program consists of. This is done by using a __Semantic Stack__ to keep track of the current statements and the current environment being processed.

We have developed the interpreter for a __Kernel Version__ of Oz. Kernel of a language is that subset of language from which the "syntactic sugar" has been stripped off.

### Features
The developed interpreter is capable of processing properly formatted programs which can consist of the following statements:

1. __Skip__
	```
	Nop
	```
	Has no effect on SAS.

2. __Variable Declaration__
	```
	LocalVar (Ident <identifier>) Statements
	```
	Allocates memory for the variable in SAS and updates the current environment as well.

3. __Variable Binding__
	```
	BindVarToVal (Ident <identifier>) (Value <value>)
	```
	For binding variable to a value.
	```
	BindVarToVar (Ident <identifier_1>) (Ident <identifier_2>)
	```
	For binding two variables.

4. __Conditional__
	```
	Conditional (Ident <boolean_identifier>) Statements Statements
	```
	The standard 'if-then-else' statements.

5. __Function Declaration__
	```
	BindVarToProc (Ident <procedure_name>) [Ident <argument>] Statements
	```
	Declares a procedure (a function with no return value).

6. __Function Application__
	```
	Apply (Ident <procedure_name>) [Ident <argument>]
	```
	Applies a procedure on given arguments.

7. __Basic Arithmetic Operations__
	```
	OperateWithVal (Ident <result>) (Ident <operand>) Operator (Value <value>)
	```
	Applies a given operator on a variable and a value.
	```
	OperateWithVar (Ident <result>) (Ident <operand_1>) Operator (Ident <operand_2>)
	```
	Applies a given operator on two variables.

8. __Record Declaration__
	```
	BindVarToRec (Ident <record_name>) <label> [(<feature_name>,(Ident <identifier_name>))]
	```
	Declares a record (which consists of a label and a set of {feature,value} pairs).

9. __Pattern Matching__
	```
	Case (Ident <record_name>) <pattern_label> [(<pattern_feature_name>,(Ident <pattern_identifier_name>))] Statements Statements
	```
	The standard 'case' statements.

Along with these features, the interpreter also performs __error checking__ at various places in accordance with the rules defined by the Oz architecture.

### Developers
_Adarsh Chauhan_, _Sai Krishna_, _Yeshi Dolma_
