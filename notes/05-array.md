# Array Programming

In a few lectures, we'll be discussing [Concurrent Languages](07-concurrent.md).
In that lecture, we'll mention _parallel languages_.
In short, parallel languages deal with _executing_ multiple things at the same time.
Array programming is a sort-of parent to parallel languages.
Most linear algebra operations involving arrays (vectors) can be parallelized.
Consider the following dot product computation:
```txt
         | 4 |
[ 1 2 3 ]| 5 | = 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
         | 6 |

         +
       / | \
      /  |  \
     /   |   \
    /    |    \
   /     |     \
  *      *      *
 / \    / \    / \
1   4  2   5  3   6
```
This is a _parallelizable_ computation.
The only dependence here is the final sum -- the multiplications can be computed (independently) on three separate compute nodes, with another node to re-combine the results for the sum.

Now, these ideas can be seen as an _implementation_ of an array programming language, but array programming languages need not be implemented concurrently nor need they support parallelism.
These are under-the-hood ideas that we should _understand exist_ (we will not implement these ideas -- take a compilers course instead), since they could potentially affect which language we choose to solve a particular problem (ex: for computation speed, compiled languages are usually faster than interpreted languages).

## What is Array Programming?

Some say that array programming is a way of thinking, others say it's a way to transparently apply applications (functions/computations) to entire wholes (arrays/matrices, instead of just scalars).
Both statements here are essentially the same.
Array programming is precisely a style of programming that allows you to apply operations on an entire set of values at once, but often solving array programming problems requires thinking more _functionally_.
The classic example presented is adding two matrices together.
In Java, we might perform this calculation as follows:
```java
for (int i = 0; i < a.length; i++) {
   for (int j = 0; j < a[i].length; j++) {
      a[i][j] += b[i][j]
   }
}
```
Whereas the exact same computation in `J` looks like this:
```j
a =: a + b
```
Cool, right?
A common theme in array programming, and really all functional languages, is _concision_ -- computations that are verbose in other high-level languages can be expressed with much less code.

Barring that, array programming can take the form of any other programming language paradigm.
Meaning, any other programming paradigm can implement some form of array programming.
As such, we leave it here for theory.

## Examples

* APL
* **J**
* K
* MATLAB

The ACM actually has a group on array-oriented languages: <http://sigapl.org/>.
It is currently alive and thriving.

## J

J is quite different than previous languages we have learned!
It looks, feels, and works differently from more imperative languages.
Not to worry, we only have to stray slightly from Justin's Learning Efficiency Guide (c).

### Brief Rundown

The main webpage for J is [here](https://www.jsoftware.com/).
The basic idea of J is that everything is an array.
Arrays hold data, like numbers, characters, arrays, and boxes.
J has a handful of types: booleans (0 or 1), integers, floats, complex numbers (`3+4i` (in math) is written as `3j4` in J), characters (strings are arrays of characters)
You perform computations by applying functions to arrays, or other functions.
Now, this is essentially the same idea as [list/meta languages](08-meta.md) -- we will discuss these a bit more in that lecture.

J was influenced by APL (A Programming Language) -- another array programming language.
APL made extensive use of unicode keywords (key-characters, I suppose).
While it looks cool, it's not the best for programming.
If you're interested, we encourage you to look at it!
We have included some [sample code](examples/apl.apl) as well.
If that doesn't satisfy your interest, you can try it at <https://tryapl.org/>.
Still not satisfied?
Check out [Emojicode](https://www.emojicode.org/).

Back to J -- let's actually _do some stuff_.

### Hello

In the console, entering any value will print said value (or value stored in the given variable).
For example, putting `'Hello, World!'` in the console will print it back to you.
We can also define a `display` idiom
```j
display =: (1!:2) & 2
```
which will display whatever the right-argument is
```j
display 'Hello, World!'
```
This is only useful when running a J file, not while _in_ the console.

### LEG (c)

Sort-of, heh.

#### Documentation

Make **extensive use** of this list:
* <https://www.jsoftware.com/> (website main page)
* <https://www.jsoftware.com/help/dictionary/vocabul.htm> (J syntax list)
* <https://code.jsoftware.com/wiki/Main_Page> (main J wiki page)
* <https://www.jsoftware.com/help/> (a list of books)
* <https://www.jsoftware.com/help/learning/contents.htm>
* <https://www.jsoftware.com/help/dictionary/dx013.htm> (debugging info)
* <http://joebo.github.io/j-emscripten/> (online J REPL)
* <https://www.reddit.com/r/apljk/> (reddit community)

#### Structure

There exists concepts like scoping, locales, multi-file scripts, etc.
We will not be discussing these -- they're outside the _scope_ (get it?) of this lecture.
Our purpose here is to discuss array programming.

We also note here that J has incredibly in-depth ideas about functions, verbs, and ways to apply them.
Just read through the many chapters in the [Learning J Book](https://www.jsoftware.com/help/learning/contents.htm)!
We will not be going too in-depth about the different ways we can apply things -- moreso just learning the basics.
It's very similar to functional programming.

#### Arithmetic/Data

Everything in J is an array, even single values (scalars).
Here's an example of the array `[1 2 3]` (tabbed lines represent console input, untabbed lines represent console output):
```txt
   a =: 1 2 3
   0 { a
1
   1{a
2
   2 { a
3
   3 { a
|index error
|   3    {a
```
Arrays have _dimension_ (standard meaning) and _rank_ (number of dimensions).
While these are important, we mostly care about _shape_.
Shape, monadic `$`, gives us the right-argument's dimensions
```txt
   $ a
3
   i. 3 4
0 1  2  3
4 5  6  7
8 9 10 11
   $ i. 3 4
3 4
   i. 2 2 2
0 1
2 3

4 5
6 7
   $ i. 2 2 2
2 2 2
```
Here, `i.` outputs an array of the inputted dimensions with each position filled in-order with natural numbers (_i._ for indices).
We can also Reshape using the dyadic form of `$` -- this puts the right-arg input into an array of the left-arg shape
```txt
   2 2 $ 1 2 3 4
1 2
3 4
   2 2 $ 1 2 3 4 5 6
1 2
3 4
   5 5 $ 1 2 3 4 5 6
1 2 3 4 5
6 1 2 3 4
5 6 1 2 3
4 5 6 1 2
3 4 5 6 1
```

Past this, we have standard arithmetic -- except, they can be applied to arrays element-wise (array-programming FTW!).
Note that negatives are represented with underscores `_` (not `-`).
Floats use periods, and imaginary numbers use `j` (eg: `5j2` is `5 + 2*sqrt(-1)` in math).
```txt
   1+2
3
   1 1 + 2 3
3 4
   1 1 + 2 3 4
|length error
|   1 1    +2 3 4
   1 1 - 2 2
_1 _1
   2 3 * 4 5
8 15
   2 3 % 4 5 NB. division
0.5 0.6
   2 3 | 4 5 NB. modulus/remainder x|y is y mod x
0 2
   7 11 <.@% 4 5 NB. integer division (a complicated combo of stuff!)
1 2
   2 3 ^ 5 6
32 729
```

We store information using `=:` (or `=.` for local variables).
```txt
   a =: 'hello!'
   a
hello!
   a =: 1 2 3
   b =: 4 5 6
   a+b
5 7 9
```

#### Control Flow

Functions!
```txt
func_name =: 3 : '... inline monad func'

func_name =: 4 : '... inline dyad func'

func_name =: 3 : 0
   y
)

func_name =: 4 : 0
   x*y
)
```

J has control flow, like
* `if. do. elseif. do. else. end.`
* `while. do. end.`
* `for. A do. B end.`
* `for_a. A do. B. end.`
* `return.`, `break.`, `continue.`
* `select. case. do. fcase. do. end.`
Most of these work as-expected.
`for_a` is interesting, though, because it allows you to iterate through an array using whatever name that appears after the `_` as the iterator.
```txt
      3 : 0''
for_item. 4 5 6 do.
  smoutput item ; item_index NB. could use our 'display' from above
end.
)
┌─┬─┐
│4│0│
└─┴─┘
┌─┬─┐
│5│1│
└─┴─┘
┌─┬─┐
│6│2│
└─┴─┘
```
More examples in [examples](examples/j).

Recursion works as-usual -- just reference the function name in the function (or `$:` if anonymous).

Logical operators are weird.
There's a set of `b.` vocabulary words, but other J primitives exist: [check here](https://code.jsoftware.com/wiki/Vocabulary/bdot#More_Information).
Basically, for boolean input: `*.` does AND, `+.` does OR, `-.` does NOT, etc.
We also have comparisons like `<, <:, =, >: >`.

One important fact about J -- most operators (keywords) are overloaded between monadic and dyadic forms.
**Monadic** means the function takes one argument (the right-arg, typically), and **Dyadic** means the function takes two arguments (one left, one right).
Example: `x < y` (dyadic) compares x and y and returns true iff x is less than y, but `< y` (monadic) "boxes up" y.
We wont use boxing, but the idea is that when you box something then it's treated as a single element.
EG: if we box a list `1 2 3` then it becomes
```txt
   $ 1 2 3
3
   < 1 2 3
┌─────┐
│1 2 3│
└─────┘
   $ < 1 2 3

   1 ; 2 ; 3
┌─┬─┬─┐
│1│2│3│
└─┴─┴─┘
   $ 1 ; 2 ; 3
3
```

There are a _lot_ of keywords (key-characters?) in J.
Remember the important ones, but just [look-up things](https://code.jsoftware.com/wiki/NuVoc) when you need to.
For example, each of
* `{`, `{.`, `{:`, `{::`
* `}`, `}.`, `}:`
* `/`, `/.`, `/:`
* `\`, `\.`, `\:`
* `=` ...
* `<` ...
* `>` ...
* `_` ...
do different things (both monadic and dyadic).
Don't memorize them, just look at provided code examples and go from there.
