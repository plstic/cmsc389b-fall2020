# Stack Based Langauges

## A Review

A stack is a data structure that works much like a stack of items. You start 
with an empty stack and add items to the top or remove items from the top. 
Stacks are usually refered to as FILO structures (*F*irst *I*n, *L*ast *O*ut) 
because you can only remove or add from the top of the stack. The data structure
should be be confused with the stack used for memory, however memory on the 
stack acts in the same manner. This is mostly because of how scope is determined
for a varaible. Recall the two operations one can use with a stack:

+ pop - removes and returns the top value of the stack
+ push - adds an item to the top of the stack

With these two operations and with another stack, you can create other common 
functions like:
+ peek - returns but does not remove the top value on the stack
+ remove - removes an item from the stack 

## Making a Language

So how can we use the stack to base a language off of? Well let us consider the
most simpler uses of a computer: a calculator. In 3rd grade I remember in math 
class we had to read this thing about the order of operations. There was a side
note and because I was a nerd, I read it. Apparently most calculators (at the 
time the book was published) use reverse Polish notation internally to calculate
values. Reverse Polish notation is basically just a fancy way of saying postfix
notation. What this means is the syntax of an experssion is operatos followed by
operator. `10 25 +` is the same as the infix notation `25 + 10` and prefix 
notation `+ 25 10` This postfix notation is analagous to using a stack. Let us 
look at the phrase `10 25 +`. What I want to do is, as I read words or values,
add them to the stack until I finish the string. Then when I want to evaluate or
find meaning in the expression, I just pop values off. That is, add 10, then 25
then '+' to the stack in that order. Then when I want to learn what `10 25 +` 
means, I pop off `+`, realize its a binary operator that adds values together, 
and so then I know pop off 2 values and those are my operands. Thus, I pop off
25 then 10 and know to `+` them together to get 25 + 10. Stack based languages
usually use stacks to pass parameters and use one or more stacks to keep track 
of data. If you recall from 216, assembly is stack based because when you push
parameters when you want to make a stack frame, you push them on like they are
in a stack. As much fun as assembly coding sounds, we will be using a different
stack based language: Forth.

## (g)Forth

Forth is a stack based language where everything is stored on and treated like a
stack. While the original Forth language is arround, we will be taking the 
easier route and will be using gforth, which is a GNU implementation of the 
forth programming language. There is more functionality to gforth, and it 
it supports more complex topics. Hence, everywhere I refer to forth, I am 
actually refering to gforth. 

While there are many intricaies of Forth such as exceptions, File I/O,
postponing and floating point values, we will only be focusing on a small subset
of these topics. As you've have hopefully learned so far, there is only so many
things you really need to be able to do in a language and still make interesting
and useful programs. 

#### stack/arithmetic operations
 + `dup` - will duplicate the top item of the stack
 + `drop`  - will remove the top item from the stack
 + `.` - will pop the top item from the stack
 + `swap` - will swap the top two items from the stack
 + `over` - will duplicate the second from the top item from the stack
 + `rot` - will place the third item of the stack on the top of the stack
 + `nip` - will remove the second from the top item from the stack
 + `tuck` - will copy the top item from the stack and palce it under the top 2 
 items

#### Functions

Forth calls their procedures colon definitions. This is because each definiton 
begins with a colon (`:`) symbol. Let us consider the following definition:

```forth
: add1 ( n -- n+1 ) 
  1 + ;
```

Following the colon, we have the definition's name `add1`. Things inclosed in 
parenthesises `(`,`)` is conisdered a comment, however it is convention to show
the what the stack looks before and after the the operation. The convention is
before and after are separated by a `--` and the left is the bottom of the 
stack. Thus, something like `swap` would be described as `( n1 n2 -- n2 n1 )`.

Definitions are then closed with a semicolon (`;`).

Additionally, Gforth allows for multiple locals to be used when defining a colon
definition (forth only allows one). Locals can be defined using `{` and `}`. 
Lets look at the two examples:

```forth
: swapEx1 { a b -- b a }
  b a ;
: swapEx2 ( x1 x2 -- x2 x1 )
  { a b } b a ;
```

The first example uses it in the stack effect comment whereas the second example
defines and uses it in the body of the definition. Both are valid, however I and
the gforth manual both suggest to avoid this when you can as it will not help 
you get into the mindset of modular programming. 

#### Conditionals

Like most programming languages, branching and conditionals can be found in 
forth. Conditionals are dependent on flags. In forth, 0 is false (or rather all
bits are 0) and -1 is true (or rather all bits are 1, and 2's compliment makes 
the value -1). However, like in c, all non-zero values are 'true'..

Common comparison operators are `=`,`<>`,`<`,`>`,`<=`,`>=` along with some 
built-in predicates like `0=` and `0<>` among others. Flags can be merged and 
modified with `and`, `or`, `xor`, and `invert`. 

There are if statements in gforth and they take the following format:

```forth
: min ( n1 n2 -- n )
  2dup < if
    drop
  else
    nip
  endif ;
```

If the value before the `if` (in this case `dup 0 <`) is a true flag then the 
code after the if will be run. The `else` branch is optional and not always 
needed (in fact you can make this function without `else`). 

Also consider the fact that these operations, `=`,`<>`,`<`,`>`,`<=`,`>=`,etc 
will consume the needed values and then place the flag on the stack.

```forth
1 2 ( put 1 then 2 on the stack )
>   ( pop off 2 and 1 then compare )
.s  ( print out the stack, in this case -1 )
```

#### Looping
All common looping constructs are available in forth. They make the use of flags
which as well. They can take the following structures: 

```forth
\ while loop
BEGIN
  code1
WHILE
  code2
REPEAT
```

`code1` is executed and if a true flag is on the top of the stack, `code2` is 
run and the loop restarts, else control continues after the `REPEAT` command.

```forth
\ do while loop
BEGIN
  code
UNTIL
```

`code` is executed and if a false flag is on the top of the stack then the loop
resets, else control continues after the `UNTIL` command.

```forth
\ for loop
limit start
?DO
  body
LOOP
```
`body` is run for all values between `start`(inclusive) and `limit` (exclusive).
To access the index between start and limit, you can use `i`, `j` and `k`. `i` 
is used for the innermost loop. Problems can arise if you are using a varaible 
of some sort called `i`, `j`, or `k`.

#### Memory

So while forth is a stack based language, it does support heap memory 
allocation. This heap can be used for either global variables or for arrays (or 
array-like structures).

##### Global Variables

Global variables can be declared using the `varaible` keyword. We can then store
and fetch values from the place in memory using `!` and `@` respectively. See 
the following example:

```forth
variable v
v . \ prints the memory address 
5 v ! \ places 5 in v and removes 5 from the stack
v @ \ will place the value in v on the top of the stack.
```

Additonally, for array structures you can allocate more than 1 'cell' in memory.

```forth
create arr 20 cells allot
```
Now I think the gforth manual says a cell is 1 byte, but I'm pretty sure that a
cell is 8 bytes, or 64 bits. I say this because you can dump the memory using:

```forth
arr 20 cells dump
```
This will dump the 20 'cells' starting at whichever memory address is stored by
`arr`. Since memory is continous and acts like an array, you can use pointer
arithmetic to access values:

```forth
389 arr 3 cells + ! \ places 389 in the cell with index 3 after arr.
arr 3 cells + @ \ pushes the value in the cell at index 3 to the stack.
```

The keyword `cell` tells you the size of the 'jump' so to speak. `arr 3 cells +` 
indicates 3 'cells' after arr. This can be contrasted with strings and 
characters.

#### Characters/Strings

So while we saw that memory can be alloacted on the heap in cells, chars in 
forth are 1 byte long which is much smaller than a 'cell'. This means we should
use the `chars` size when we want to do things with strings: 

```forth
\ I'm like pretty sure these two will allot the same amount of space
create str 16 chars allot
create str2 2 cells allot

\ to add a string we want to use the 'chars' keyword
97 str !
98 str 1 chars + !

str 2 type \ will print 2 chracters which are stored in 'str'.
str 2 chars dump \ should do the same thing but also show the actual memory
