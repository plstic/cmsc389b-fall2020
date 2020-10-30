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

For this project you will be creating a Racket to C compiler. Kinda. You will
be taking in very simple math operations expressions, valid in racket, and 
then returning a c program which is analgous. I will then be compiling the 
returned program and the program should print out what the expression evaluates
to. Do not worry, as I have done most of this for you. You just need to decide
how you want to parse the racket expression and express it in C.

## Valid expression

You only need to worry about a small number of exressions. The language is as
follows:

+ Number - just a single number is valid
+ addition - (+ expr expr) is a valid expression
+ subtraction - (- expr expr) is a valid expression
+ multiplication - (\* expr expr) is a valid expression
+ division - (/ expr expr) is a valid expression
+ absolute value - (abs expr ) is a valid expression

You may find the `(append lst1 lst2 ... )` procedure helpful for this project.

You may assume that input will always be valid. You can test the public tsets 
by running `make test`. 

I gave you the code for compiling down a number. Notice I just give back a list
of the number. Thus, when I call compile, on line 10, the append will add 
whatever value is in the list to the c program.

I would highly suggest watching the project video for this project. 

For a hint if there was such thing as `(sq expr)` in this language, this is how
I would go about it:

```Racket
(match e
	[`(sq ,exp) `(,exp * ,exp)])
```
## submission

Submit `calc.rkt`, `main.rkt` 
