# hoz
## A Semantic Interpreter for Oz Kernel Language

# Specifications
A program in Oz is just a statement. Any statement in Oz is represented as a list (in Oz). In general, the source code is given as an __Abstract Syntax Tree__ (AST), which is represented as a nested list.

We have implemented an interpreter which takes such an AST as input and outputs the effect of the program in terms of the memory allocations it does.

Oz consists of a __Single Assignment Store__ (SAS), like an abstract form of memory, where variables are allocated space and values of variables are stored. We have interpreted the given program by __Operational Semantics__, which is essentially an exhaustive enumeration of the steps which a program consists of. This is done by using a __Semantic Stack__ to keep track of the current statements and the current environment being processed.

We have developed the interpreter for a __Kernel Version__ of Oz. Kernel of a language is that subset of language from which the "syntactic sugar" has been stripped off.

# Features
The developed interpreter is capable of processing properly formatted programs which can consist of the following statements:

1. _Skip_
```
Nop
```
Has no effect the SAS.

2. _Variable Declaration_
```
LocalVar (Ident <identifier>) [Statements]
```
Allocates memory for the variable in the SAS and updates the current environment as well.

3. _Variable Binding_
```
BindVarToVal (Ident <identifier>) (Value <value>)
```
For binding variable to a value.
```
BindVarToVar (Ident <identifier_1>) (Ident <identifier_2>)
```
For binding two variables.

4. _Conditional_
```
Conditional (Ident <boolean_identifier>) [Statements] [Statements]
```
If-then-else statements.

5. _Function Declaration_
```
BindVarToProc (Ident <procedure_name>) (Ident <argument>) [Statements]
```
Declares a procedure (a function with no return value).

6. _Function Application_
```
Apply (Ident <procedure_name>) (Ident <argument>)
```
Applies a procedure on given arguments.

7. _Basic Arithmetic Operations_
```
OperateWithVal (Ident <result>) (Ident <operand>) Operator (Value <value>)
```
Applies a given operator on a variable and a value.
```
OperatorWithVar (Ident <result>) (Ident <operand_1>) Operator (Ident <operand_2>)
```
Applies a given operator on two variables.

Along with these features, the interpreter also performs error checking at various places in accordance to the rules defined by Oz architecture.
