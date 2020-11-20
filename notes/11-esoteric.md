# Esoteric Languages

## Esolangs

Let us first consider: what is a computer? I would argue the modern computer, 
based on Alan Turing 1936 paper is anything which holds the properties of a 
Universal Turing Machine. However, I believe computers, that is a device to help
computations, have been around for thousands of years. In fact, us using fingers
to help add numbers or keep track of data could be considered tools to help with
computations. But we don't want to go that far back. No, I would start with the
first programmable computer. 

According to Wiki, this idea originated from Charles Babbage in the 19th Century
. In fact, his proposed device would be considered Turing-complete. He 
ultimately was unable to create the device due to lack of funding from the 
British Government. What's up with the British not liking computers? Anyway, 
this idea of a programmable computer would mean that there had to be a way to 
interface with and program the device. While, this usually started out with 
just logic based programming (as we had to mimic the mechanical hardware), we 
have since evolved and can now make higher level languages where we can abstract 
away this low level aspect of the command languages used. 

As we learned in the flex and bison lecture, we can compile languages down to 
the assembly or machine code needed for our computers to understand what we want
done. As you probably wondered, if we can make a lexer or parser for anything, 
could we make a language that consists only of spaces, tabs and carriage 
returns? Yes. Yes we can. It's called 
[whitespace](https://esolangs.org/wiki/Whitespace). Like, why should we be 
confined to proper words like 'if' or 'then' or 'while' or 'true' and 'false'? 
As we know, C doesn't have 'true' or 'false'. Remember, a language is just a 
defined grammar and syntax to convey information. If we write a compiler to take 
in certain keywords or syntaxes, as long as we know how to evaluate the 
statement, then we are good to go.

This is where esoteric languages come into play. They are made as a proof of 
concept, to demonstrate how little we really need to make a Turing complete 
language. Or as Jokes, or intermediate interfaces for more complex operations 
like hacking. Scripting is a crude version of this. I can bind multiple commands 
to a script and just run that one script. Aliasing is similar. So what crazy 
things have people made?

Well we all know of brainfuck, a language that's based on a Turing machine, with
only 8 characters that make up the language. However, I want to focus on the 
first esoteric language known as INTERCAL.

## INTERCAL
Don Woods and James M Lyon designed INTERCAl in 1972 to parody common elements 
from the popular languages at the time: Fortran, COBOL, and assembly. 
The language only existed on paper until the 1990s when it was implemented in C.
The revival of this language sparked the growth and rapid development of other 
esoteric languages. 

So what makes INTERCAL an esoteric language? Honestly, all you need to know 
about how to use and write INTERCAL programs can be found in the 
[manual](intercal.pdf). However I will share some of the most import parts of 
the manual here. At the bottom, I'll have some example programs and go over what
they do..

### The pure programming power of INTERCAL

> *The full name of the compiler is "Compiler Language with no Pronouceable 
Acronym" which is, for obvious reasons, abbreviated "INTERCAL."*

#### Description

 + Variables: can only be 16 or 32 bit integers. 
    + 16 bit variable names are written using the spot character(`.`) followed 
    by any number between 1 and 65535, inclusive
    + 32 bit variable names are written using the double spot character(`:`) 
    followed by any number between 1 and 65535, inclusive

  Thus there can only be 131070 variables allowed in a program. Additionally, 
  these variables can only contain non-negative values.   
 + Constants: can only be values from the range 0 - 65535 inclusive. They are 
 preceded by the mesh(`#`) character. 
 + Arrays: arrays can hold 16 or 32 bit values.
    + 16 bit arrays are written using the tail character(`,`) followed by any
    number between 1 and 65535, inclusive
    + 32 bit arrays are written using the hybrid character(`;`) followed by any
    number between 1 and 65535, inclusive
    + array subscripts can be accessed by suffixing `SUB` to the end of the 
    array name followed by the subscript.

So in conclusion, the naming scheme for variables, constants and arrays are all
the same except the prefixed character determines its type. Thus, `.123`, `:123`
, `#123`, `,123`, and `;123` are all distinct.

  + There are 2 binary operators: interleave (aka mingle) `¢` and select`~`
  .
    + Interleave takes two, 16-bit integers and produces a 32-bit integer of 
    which is the results of alternating the bits in the 2 operands
    + Select will take whichever bits correspond to 1's in the second operand, 
    then places these bits to the right and pads with 0s to make either a 16 or
    32 bit integer, depending how many bits were selected.  

Thus, `#65535 ¢ 0` will produce 2863311530, and `#179~#201` will produce 9.

  + There are 3 unary operators: `&` (logical and), `V` (logical or), `∀`
    (logical xor).
    + the operator is placed between the type-symbol (mesh, tail, hybrid,etc) 
    and the varaible name.
    + Each unary operator works by shifting the value 1 bit to the right (with 
    wrap around) and then doing the local operateration with the original value.
    + Unary operators cannot be concatenated.

Thus `#&77` will produce 4,  `#V77` will produce 32879 and `#∀77` will 
produce 32875.
 
  + There are no precedence rules so you must mark which operations you want to
  be calculated first by putting them in-between sparks(`'`) or rabbit ears(`"`).
  
> Thus `'#165¢#203'~#358` (binary value '10100101¢11001011'~101100110) has the
value 15, but `#165¢'#203~#358'` has the value 34815, and `#165¢#203~#358` is
invalid syntax and is completely valueless (except perhaps as an educational 
tool to the programmer).


#### Statements

  + Statements usually will ignore whitespace although it is recommended to 
  place each command on a separate line and to add spaces to reduce program 
  illegibility
  + Each statement can begin with a label which can be used for identification 
  and reference purposes (we will see examples later).
    + like everything else, labels can be any number between 1 and 65535 
    inclusive
    + They are enclosed in wax-wane pairs (`()`).
    + Some labels are used for [INTERCAL system libraries](#system-libraries) so
    it is reccomended to not use these labels.
  + After any labels, if any, one of the three statement identifiers must be 
  used: `DO`, `PLEASE`, `PLEASE DO`.
  + The identifier is then followed by either, neither, or both of the following
  qualifiers:
    + `NOT`, `N'T`: Will make the line abstained
    + A double-oh-seven (`%`) followed by a number between 1 and 100 inclusive. 
    This will cause this line to have that percent chance of being executed 
    during runtime.
#### Operations

There are 13 valid operations:
  + Assignment: To assign a variable a value, we use the angle symbol followed 
  by the worm (`<-`). 
    + To define a multidimensional array, you separate the size of each 
      dimension with the `BY` keyword.
  + Subroutine and unconditional calls: Use the `DO (label) NEXT` syntax.
    + Using `NEXT` will place a return location to the push-down list.
    + Attempting to add an 80th value to the push-down list will cause the 
    `PROGRAM HAS DISSAPPEARED INTO THE BLACK LAGOON` fatal error message.
    + You must remove values from the push-down list yourself. 
  + Removing values from the push-down list: Use the `PLEASE FORGET (exp)` 
  statement to do this.
  + Returning from a next: Use the `PLEASE RESUME (exp)` statement. This has the
  same affect of the `FORGET` statement but control is returned to the statement
  preceding the last `NEXT` call stored on the push-down list.
  + Stashing, suppose that a subroutine is recursive or multiple use the same
  variable. You need a way to not overwrite data. This can be done with the 
  `STASH` command. 
    + You can stash multiple values by separating variables with intersections
    (`+`)
  + Retrieving stashed values: You can retrieve stashed values by using the 
  `RETRIEVE` command
    + You can stash the same variable multiple times and `RETRIEVE` will 
    retrieve the last stashed value
    + Trying to retrieve a non-stashed value will cause the `THROW STICK BEFORE
    RERTIEVING` error.
  + Ignoring: If you want to make a value immutable you can use the `IGNORE` 
  command. This means all subsequent statements to have no effect on the 
  variables/arrays 
  + Remembering: If you want to make an immutable variable mutable, you can use
  the `REMEMBER` command.
  + Abstain: Like ignoring, if can decide to cause certain lines from running or
  even statements of certain types. 
    + To prevent a statement from running, use `DO ABSTAIN FROM (label)`
    + To prevent a statement of a type from running, use `PLEASE ABSTAIN FROM 
    (gerund list)`
    + `PLEASE ASBTAIN FROM IGNORING + FORGETTING` will prevent subsequent 
    ignore and forget statements from taking affect
  + Reinstate: This will reverse any `ABSTAIN` statements used previously.
  + Exiting: To terminate a program you can use `PLEASE GIVE UP` or `PLEASE 
  RESUME #80`.
  + Input: To read in, use `DO WRITE IN (list)` where list is a list of 
    variables
    + Input is numbers and must be written out. So if you want to input 15, you
    would write `ONE FIVE`.
  + Output: to print something out, use `DO READ OUT (label)`. Have fun with 
  output.

If a line is not formatted correctly, its considered a comment. It will only 
cause an error if that line is reached during runtime.

#### System Libraries

See page 12 of the [manual](intercal.pdf) 

#### Errors/Compiler Errors

See page 20 of the [manual](intercal.pdf) 



### Examples

So while I suspect that this all makes intuitive sense, let's go over some 
example code.

#### Hello World
I propose we start with the simple "Hello World" program. 

```INTERCAL
DO ,1 <- #13
PLEASE DO ,1 SUB #1 <- #238
DO ,1 SUB #2 <- #108
DO ,1 SUB #3 <- #112
DO ,1 SUB #4 <- #0
DO ,1 SUB #5 <- #64
DO ,1 SUB #6 <- #194
DO ,1 SUB #7 <- #48
PLEASE DO ,1 SUB #8 <- #22
DO ,1 SUB #9 <- #248
DO ,1 SUB #10 <- #168
DO ,1 SUB #11 <- #24
DO ,1 SUB #12 <- #16
DO ,1 SUB #13 <- #162
PLEASE READ OUT ,1
PLEASE GIVE UP
```

Here, the first line and last two lines are the important ones. The first line 
makes an array of 13 long. The next 13 lines say, at the array `.1`, at some 
index (starting at 1, gross), place the characters for "Hello World!". The 
penultimate lie says to print out the array and the last line says to terminate
the program. 

To compile this program, you use the `ick` command, the name of the compiler is
`ick`. If you run into error 774, just recompile.

Now, if you are wondering why none of these number repeat, that is, "Hello
World!" has a double "ll", why don't we see a double number, that's because 
INTERCAL builds strings in this way:

+ Bit-reverse ASCII-code of previous printed character (assuming it’s 8-bit) to 
get rev(i-1). When outputting first element of the array, this is assumed to be 0.
+ Get the i-th element of the array array(i).
+ Subtract array(i) from rev(i-1) to get rev(i).
+ Bit-reverse rev(i) to get i, the ASCII-code of the character to be printed.

Example to print "He":

We now just reverse engineer these steps. We want to print out "H" or bit 
string "01001000". We reverse it to get "00010010" (what I called rev(i)).
We know since "H" is the first string, the previous value is just 0 or 
"0000000", which bit reversed it still "00000000" (what I call rev(i-1)).
We now just need to solve "00000000" - X = "00010010". This is, if we recall
CMSC216, "11101110" (Technically "11101110" + "00010010" = "100000000" but we 
are working with 1 byte characters so the preceeding 1 is cut off). And if we
recall our binary to decimal conversions, then we know that "11101110" is 238.

Basically, i = "01001000", rev(i) = "00010010", rev(i-1) = "00000000", i-1 = 
"00000000". "00000000" - X = "00010010" (same thing as "00000000" - "00010010" =
X, hence X  must equal "11101110" or 238.

Now, to add "e" to the mix. The previous printed value's ascii binary value is
"01001000" (this is not the value we put into the array, but the printed value's
ascii value).  We reverse this to get "00010010". We want to print 'e' or 
"01100101", which reversed is "10100110". We then solve "00010010" - X = 
"10100110" or for simplicity "00010010" - "10100110" = X. X is "01101100"
because of 2's compliment. "01101100" in decimal is 108.

Basically, i = "01100101", rev(i) = "10100110", rev(i-1) = "00010010", i-1 = 
"01001000". "00010010" - "10100110" = X, X equals "01101100" or 108.

#### Add

Okay cool, so lets move onto just adding two integers together.

```INTERCAL
DO WRITE IN :1+:2
DO (1500) NEXT
DO READ OUT :3
PLEASE GIVE UP
```

This one is straight forward. We write in two integers, and store them in `:1` 
an `:2`.  We then call the built in add function with `DO (1500) NEXT`, which 
can be found in section 5.2 of the [manual](intercal.pdf). This subroutine will
put the resulting value in `:3` assuming no overflow and then we read it out. As
usual, we then terminate by giving up.

#### Looping with Fibonacci

This is pretty cool but like, I have a smooth brane so it took me a while to 
learned how this worked. This is going to act like a for loop, but honestly
a for loop is just a while with a built in change.

```INTERCAL
   DO .9 <- #5
    DO .10 <- #0
    DO .11 <- #1

(1) PLEASE READ OUT .11
    DO .1 <- .10
    DO .2 <- .11
    PLEASE (1009) NEXT
    DO .10 <- .11
    DO .11 <- .3

    DO (3) NEXT
    DO (1) NEXT

(3) DO (4) NEXT
    PLEASE GIVE UP

(4) DO .1 <- .9
    DO .2 <- #1
    PLEASE (1010) NEXT
    DO .9 <- .3
    DO .1 <- '.9~.9'~#1
    PLEASE (1020) NEXT
    DO RESUME .1
```

The first three lines, is the setup. Put 5 into var `.9` because I want to print
out the first 5 numbers of the fibonacci sequence. This is going to act as the
counter. Then place 0 and 1 into vars '`.10` and `.11`. This is the first "two"
values. I say this because I start fibonacci with "1, 1, 2, 3, 5" so I'm not 
going to actually print out 0. 

I then make the body of my loop. I first am going to print out the value stored 
in `.11`. This will initially be 1. After that I am copying `.10` and `.11` into
`.1` and `.2`. This is so I can call the built in subroutine (1009) which is 
just add but for the `.` type instead of the `:` type. I then update the two 
values of `.10` and `.11`. In this variant of fibonacci, you only need to keep
two values in memory, current and last. After this, comes the important lines
of:

```
DO (3) NEXT
DO (1) NEXT
```

This will say, go to subroutine (3) and then when/if you return here, restart 
the body.

We then need to look at subroutine (3) which is pretty simple: Go to subroutine
(4) and then end the program. This may seem odd, but don't worry, it'll make 
sense in subroutine (4) why we did this.

Subroutine (4) is where we update the counter, and check the guard. 
Understanding this is important. The first line puts the counter in `.1`. The 
second line puts 1 into `.2`. We then call the (1010) subroutine to subtract 
`.2` from `.1` and have the answer be put into `.3`. We then update the counter,
by placing `.3`'s value into `.9`. The next statement uses the select operator, 
(see binary operators), to place the value of 1 into `.1` if `.9` is non-zero or
puts 0 into `.1` if `.9` is zero. We then call subroutine (1020) which will add
1 to `.1`. Thus, if the counter is 0, then `.1` holds `, otherwise `.1` holds 2.
Now we resume the value of `.1`. So if the counter is zero, we resume 1, else 
resume 2. This just means, how many `next` statements do we go back by. If we 
go back one, we end up in subroutine (3) and then we give up. Else we end up in 
subroutine (1) and we redo subroutine (1) and redo the loop.

That's it. EZ.
