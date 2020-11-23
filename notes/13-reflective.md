# Reflective Programming

Sometimes you can write code that references other code you have written.

> Wait, what?

Reflective programming is the idea of introspection.
Basically, a language is reflective if it can examine and modify its own structure at runtime.
If your language can access the _type_ of some data, it is reflective.
If your language can modify its own source code and run something else, it is reflective.

Reflective languages allow you to
* inspect types
* match strings to symbolic names
* interpret strings as source code
* modify objects

The concept is relatively simple.
We follow up with some examples that illustrate the point.

## Examples

Most higher-level languages implement some form of reflection.
Some examples:
* **Befunge**
* C#
* Go
* Java
* Prolog
* Python
* Ruby
* and others...

For example, Python includes methods like `getattr` and `setattr` to examine whether some object has a field/method and set some method/variable as a field:
```python
class String:
    def __init__(self, s=""):
        self.s = s
    def __repr__(self):
        return self.s
    def set_str(self, s):
        self.s = s
    def get_str(self):
        return self.s
    def is_empty(self):
        return len(self.s) == 0
    #
#

# standard
s = String()
b = s.is_empty()

# equivalent, with reflection
s = globals()["String"]()
getattr(s, "is_empty")()
# note the () at the end say to invoke the result

# equivalent, using eval
eval("String().is_empty()")

# setting
s = globals()["String"]()
setattr(s, 'test1', 500)
setattr(s, 'test2', lambda x: getattr(x, 's', None))
dir(s)
```

Java has `java.lang.reflect.Method`:
```java
import java.lang.reflect.Method;

// standard
String s = new String();
Boolean b = s.isEmpty();

// equivalent, using reflection
try {
    Object s = String.class.newInstance();
    Method m = s.getClass().getDeclaredMethod("isEmpty", new Class<?>[0]);
    Boolean b = (Boolean) m.invoke(s); // Method.invoke() -> Object
} catch (ReflectiveOperationException ignored) {}
```

Cool.

## Program Transformations

Reflection is a typcal feature of higher-level languages, but it is hard to implement for lower-level languages like C.
If a language does not support reflection, then one can implement reflection using a _program transformation_.
A program transformation is just an operation that transforms one program to another program -- essentially, a compiler but more abstract.
In the case of reflection, we could implement a program transformation that handles type look-ups and source-code changes.

Some good information about reflection in C here: <https://stackoverflow.com/q/1353022>.
Implementing reflection for lower-level languages is difficult and unnecessary.
If you are interested in this, consider doing research in the **program analysis** area of computer science.

## Befunge

Pronounced **bee-FUNJ**, Befunge is an esoteric programming language that is highly reflective.
In fact, the entire point of Befunge is to be reflective.
Well, really the point of Befunge was to be difficult to compile.
Same thing, really.

### Intro

Befunge takes a "playfield" -- a 2D matrix of text characters -- as input, and returns a calculated result.
[The webpage](https://catseye.tc/view/Befunge-93/doc/Befunge-93.markdown) introduces the entire language succinctly.
[The Esolang webpage](https://esolangs.org/wiki/Befunge) also introduces the language succinctly.

Befunge has two versions -- befunge-93 and befunge-98.
The main difference is that 98 allows for infinite playfield area.
We will use befunge-93 for now.

Befunge works by having a stack and program counter (PC).
The stack is a stack -- you have seen these when working with `forth`.
The PC just says where you are in the code.
With normal languages, the PC is sequential.
With Befunge, the PC is any position on the playfield and can be moved in any cardinal direction.

### Resources

Multiple Befunge interpreters exist online that will show you how your program executes.
* <https://www.bedroomlan.org/tools/befunge-playground>.
* <http://qiao.github.io/javascript-playground/visual-befunge93-interpreter/>
* <https://befunge.flogisoft.com/>
Very useful for understanding what is going on.

### Commands

We start by showing you the commands.
There are not many!
This language is super simple.
With simplicity, however, comes complexity.

Stolen from the creator's website:
```txt
COMMAND         INITIAL STACK (bot->top)RESULT (STACK)
-------         -------------           -----------------
+ (add)         <value1> <value2>       <value1 + value2>
- (subtract)    <value1> <value2>       <value1 - value2>
* (multiply)    <value1> <value2>       <value1 * value2>
/ (divide)      <value1> <value2>       <value1 / value2> (nb. integer)
% (modulo)      <value1> <value2>       <value1 mod value2>
! (not)         <value>                 <0 if value non-zero, 1 otherwise>
` (greater)     <value1> <value2>       <1 if value1 > value2, 0 otherwise>
> (right)                               PC -> right
< (left)                                PC -> left
^ (up)                                  PC -> up
v (down)                                PC -> down
? (random)                              PC -> right? left? up? down? ???
_ (horizontal if) <boolean value>       PC->left if <value>, else PC->right
| (vertical if)   <boolean value>       PC->up if <value>, else PC->down
" (stringmode)                          Toggles 'stringmode'
: (dup)         <value>                 <value> <value>
\ (swap)        <value1> <value2>       <value2> <value1>
$ (pop)         <value>                 pops <value> but does nothing
. (output int)  <value>                 outputs <value> as integer
, (output char) <value>                 outputs <value> as ASCII
# (bridge)                              'jumps' PC one farther; skips
                                        over next command
g (get)         <x> <y>                 <value at (x,y)>
p (put)         <value> <x> <y>         puts <value> at (x,y)
& (input int)                           <value user entered>
~ (input character)                     <character user entered>
@ (end)                                 ends program
```
also note that putting any integer 0-9 will put that integer onto the stack.

### Hello

Basic hello world:
```befunge
64+"!dlroW ,olleH">:#,_@
```

The PC starts at the top-left.
It puts 6 on the stack, then 4, then adds them to get 10.
Note that 10 equals a newline character in ASCII.
Then, the PC adds `Hello, World!` onto the stack, ASCII-version, in reverse order.
`>:#,_@` is the interesting part here.
We go right, duplicate the top stack value, skip the comma, then test the top stack value for boolean true.
0 is the only false value (or an empty stack), so every other value will be considered true.
The `_` consumes the top value -- if true points the PC to the `<` left, if false points the PC to the `>` right.
On every iteration until the stack is cleared, the branch evaluates true so goes left.
Then the comma prints the top stack value (remember, it was duplicated), we skip over the duplication command again, and the `>` points the PC back to the right.
So we repeat.

Cool!

### Less is More

Another example:
```befunge
vv  <      <                                                                   
    2                                                                          
    ^  v<                                                                      
 v1<?>3v4                                                                      
    ^   ^                                                                      
>  >?>  ?>5^                                                                   
    v   v                                                                      
 v9<?>7v6                                                                      
    v  v<                                                                      
    8                                                                          
    >  >   ^                                                                   
 vv  <      <                                                                  
     2                                                                         
     ^  v<                                                                     
  v1<?>3v4                                                                     
     ^   ^                                                                     
 >  >?>  ?>5^                                                                  
     v   v      v          ,*25         <<                                     
  v9<?>7v6                              ,,                                     
     v  v<                              ""                                     
     8                                  ><                                     
     >  >   ^                           ""v                                    
  >*: >0"!rebmun tupnI">:#,_$25*,:&:99p`|^<       _0"!niw uoY">:#,_$25*,@      
      ^         <                       >:99g01-*+^
```

The interesting part of this example is the random number generator.
We have two "loops" going on here.
We start going down, then we go right and enter a `?` which sends the PC in a random direction.
If we go north, then we go into another `?` which puts us at 1, 2, or 3.
If we go east, then we get 4, 5, or 6.
If we go south, then we get 7, 8, or 9.
If we go west, then we go back to that same first `?`.
After the first number is selected, all paths point the PC to a second loop RNG.
The second loop works as the first.
Then we go to a printer `>:#,_` after putting `Input number!` on the stack.
`&` gets integer input from the user.

Now comes the interesting part.
We duplicate the value, and put two 9s on the stack.
Then `p` -- we `p`ut the duplicated stack value on the playfield in position 9, 9.
This saves the value for later, since the `p` eats the value.
Then the backtick tests if the random value is greater than the user value.
This eats the user value.
If greater, the `|` conditional branches north, prints that the random value is greater, and repeats.
Else, it branches south and we `g`et the value from position 9 9.
We had to save this because we ate it earlier.
Once we retrieve it, then we add the negative of the user value to the random value.
If non-zero, then the random value is less, otherwise the random value is equal.
The `_` conditional reflects this.

### Reflection

Right -- the whole point of this language.
You can put and get values that appear on the playfield using `p` and `g`.
This is reflection -- we are inspecting and modifying the source code in this way.
We saw this in the previous example, but let's see another.

Note that `p` and `g` work using (x,y) grid positions.
The top-left of the grid is position (0,0).
x-position goes up and down.
y-position goes left and right.

```befunge
v  1   >$0Xv   v                                    <
>&:19+`|v Y<            >v      5      6    7     8
^  \   <>09p19p>09g+09p:|>2*:19+%19g+19p19+/19g+19p:|
   2          3      4  >                           v
                              v"invalid"<10  9
                                        |%+91+g91g90<
                              v  "valid"<
                              >:#,_@
                                 11
```
<https://rosettacode.org/wiki/Luhn_test_of_credit_card_numbers#Befunge>

This example implements Luhn's test, which takes an input number and:
1. reverse the order of the digits
1. sum together the odd-indexed (index by 1, not 0) digits into `s1`
1. for each even-indexed digit
   1. multiply by 2
   1. sum the result's digits (nothing to do here if the result is less than 10)
   1. sum this result to `s2`
1. output `valid` iff `s1+s2 % 10 == 0` (ends in zero)

There are labels 1-11 in the actual source code, as well as an X and Y.
The X and Y are the playfield accumulator positions of `s1` and `s2` respectively.
Note that the actual X and Y cannot be there, because they will interfere with the PC -- X and Y are "unsupported instruction"s -- the example code does not include the X and Y for this reason.
1. input numbers one-by-one until a number greater than 9 is given
2. mostly unnecessary -- is needed because the first swap `\` adds a zero, so we have to keep swapping the zero up
3. we place a zero on the stack (so now there are two zeros), and place those into the playfield at X and Y
4. get X, add to the top stack value (the first digit, reversed), replace to X, then duplicate the top stack value (the second digit, reversed), compare for non-zero (go north if true), multiply digit by 2, duplicate
5. mod-10 that result, get Y, add to Y, put back to Y
6. integer divide by 10 (to get the second digit)
7. get Y, add to the result of step 6, put back to Y
8. the top stack value is the next user-inputted value, so duplicate it to see if it exists. if it does, go north to repeat our algorithm
9. else, we go south, get X, get Y, add them, and compute the result mod 10
10. if non-zero mod10, then we go north (true) and put "invalid" on the stack, otherwise it is divisible mod10 and we go south to put "valid" on the stack
11. standard printer to print the stack value

Notice that if there are an odd number of digits, then the second part (4.5-7) just results in adding zero (an empty stack yields zero).
