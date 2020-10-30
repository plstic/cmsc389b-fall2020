# Racket Compiler

Due 15 November 2020, 11:59pm EDT

Points: 50 Public, 50 Semipublic

## Before you Start

Please make sure that if you are not using docker, or have updated the docker
image, that you are using at least `Racket 7.8`. You can check this by running
`racket`. If the command does not run then you do not have it installed. 
Additionally, you could install DR. Racket for a full fledged IDE with REPL 
included.

## Introduction
You will be making a small compiler for a small math language which uses
in-fix notation. I have this setup slightly differnt from the simp-calc implementation
from the examples because we added negative numbers, and floats. Additionally, 
the backend is a binary tree and so you'll need to use a few functions in the 
`big-brane-calc.c` file. I will expand on this in the next section, but please
watch the project video if you get confused. 

You may assume that input will always be valid. You can test the public tsets 
by running `make test`. 

## Backend Implementation
I have an AST binary tree which I will be storing the expressions. In the Bison
file I would say that when an expression of the grammar is matched, you should
create a new ast for each grammar branch because at the end I am going to call
my eval function on the root of the AST. Each Node of the AST had an int value
that tells the type of node (is it a number or an expression node, and what type
of expression is it?). Due to how C stores things in memeory, we can do some
cool things like have the int type in both an AST struct and a number struct 
and depending on the value in the int, treat the node as either AST or number.

Either way, you can create an ast node by calling the `newast(type, astnode, astnode)`
function. You can then create a number node by calling `newnum(double)`. 

You will need to create a lexer, much like we did in the `simp-calc` example,
but just include floats. In the bison file, you will need to build the ast tree, 
rather than evaluate the expression.

## Valid expression

A valid expression in this compiler can be anything of the following:

+ expr + expr - addition
+ expr - expr - subtraction
+ expr * expr - multiplication
+ expr / expr - division
+ |expr| - absolute value
+ (expr) - order of operations
+ Integer - any 32 bit signed integer (-3, 3, 5, 6, 123)
+ Float - any small float value in the form of something like 12.2, .12, or 0.4. 

Hint: If i were to do this, I would do something like this
```
| exp ADD exp { $$ = newast('+', $1, $3); }
| NUMBER { $$ = newnum($1) } //we can do this because we should set yylval.d to whatever value the token's value is.
```

## Submission

Submit `big-brane-calc.c`, `big-brane-calc.h`, `big-brane-calc.y`, `big-brane-calc.l`
