### M E T A

This week will be working on Meta-Languages. Meta-programming is the cool idea
that we can treat code as data. This is different than treating functions as 
data as you may have seen in Ocaml or C with function pointers. In a 
meta-language, the language can easily produce or read in other programs as 
data. This can create self-modifying programs, interpreters or compilers.
This week we will be making a compiler where we will be taking Racket code, 
and compiling the code C code. Next week we will be looking a compiler 
constructors.

# Lisp based languages

In a dumbed down, overly simplified, and in a ELI5 manner, Lisp languages are
languages where things are treated as lists. I'm sure that if anyone who uses
these languages in heard this, they would cry and say that is incorrect (sorry 
David Van Horn). In reality, Lisp is based around the linked list data structure
and since it is a meta language, everything can be thought of a list in a sense,
and since code and data are easily interchanged, when you are both modifying 
data and writing code, you are modifying lists. 

Now, Lisp is pretty old, so we will be coding in Racket, a decendent of Scheme
which in turn is a decendent of Lisp. If you ever coded in Lisp or any Lisp 
like language, you will know that there are many parenthesis and matching them 
can be a pain. Racket is no different. 

## Racket

Racket acts like any other functional programming language that I have come across
and acts similarly to Lisp in the sense that it uses prefix notation. It also 
uses a ton of parenthesis to help define a function. The interesting part of 
Meta languages make more sense once we get a feel for the language. So lets go 
over basic examples of all the parts you need to know of Racket before we get to
the meta aspects. 

### Primitives and Operations

So lets start out with the bare basics of this language. Remember, for this class
you do not need to be fluent in this language. I would say that you don't need to
memorize or know the syntax in depth for this class. That is because, a 
programming language is just that, a language. It's nice to know how to express
yourself in many ways but ultimately, you can express everything in a very 
small subset of the language, and I feel that with coding, this is especially 
true. 

```Racket
; we have basic primitive values of ints, floats, booleans, strings, chars.
; comments are started with semicolons.
; ints, strings, floats and booleans are basically the same as in every other
; language. However Booleans can also be expressed as #t and #f.
1
1.0
"cmsc389b"
#t
true
#f
false
; characters are expressed slightly differently. They take the form \# if a
; literal, or they can take in unicode values with \#u. Additionally, 
; there are special character codes like #\space or #\newline
#\A	
#\a
#\u03BB
#\2
```

Unlike in other languages you may have used before, parenthesis are part of the
expression of a function. Also remember racket is functional, and we consider
operations to also be functions so we treat them as functions and hence get this
prefix notation.
```Racket
; + is a function, like had I made a function called add and then called it like
; add(1,2).
(+ 1 2)
(+ 1 (* 3 4))
; we use parentheseis to indicate the begiining of a function. Using this
; notation makes it easier, in my opinion, to see how everything is a list.
(char? #\A)
(integer? 23)
; If you really think about it, these are 2 lists with 2 elements in them. The
; first element is the function, and the subsequent elements are the parameters.
; This is how code can be treated as data. 
; Note: parentheses indicate a function so we cannot do
(1)
; as Racket will complain about not being able to find a fuction named 1.
; Finally, you may want to use let expressions for local bindings
(let ((x 3)) x))
; notice that we can take in a list of bindings in let. Each of these are
; independent of each other and so if something has a side effect, it won't
; affect the other bindings
(let ((x 3) (y (+ 2 x))) (+ x y)) ; will fail becuase it doesn't know what x is.
```
So lets consider making an anonymous function that takes 2 values and adds them.
Remember that things can be though of lists.
```Racket
(lambda (x y) (+ x y))
; the list has 3 things, a lambda character indicating this to be an anonymous
; function. Then a list of arguments. Then a list of what you want the procedure
; to acomplish.
; Let us then consider how we do a function application
((lambda (x y) (+ x y)) 2 3)
; So this is a list of function and arguments. Its just the function is being 
; defined instead of referenced to via name.
; Suppose we defined the function in a file
(define (my-add x y) (+ x y))
(my-add 2 3)
; If we consider that everything here is a list, you can see we will always look
; at the first element of the list to understand or figure out what the 
; subsequent elements represent. Define says a list consisting of a a name and 
; parameters will follow, then a list of operations you want to occur.
```

### Conditions

So like most languages, Racket offers an easy control flow operators. We will 
be focusing on the `if` and `cond` procedures. The `cond` is like a big 
if-elseif-else chain but you can of course chain `if` expressions together.

```Racket
(if (= 1 1) 1 0)
(if (= 0 1) #f (if (= 1 1) #t #f))
(if (equal? 1 (+ 2 (- 2 -3))) #t #f)

; if caluses must have an else condition

(let ((x '(1 2 3 4 5)))
	(cond x
		[(equal? x '(1 2 3))	0]
		[(length x)		1]				; if not false then true
		[#t  0]))
; this will stop once a true condition is reached
```

### Lists
Now if everything is a list, then how do make a list in racket?
Well Racket has a built in list datatype. We use this idea of quoting to 
represent lists.

```Racket
'() ; empty list
(list 1 2 3) ; is the same as
(cons 1 (cons 2 (cons 3 '()))) ; cons is analygous to Ocaml 1::[]
; Notice how I had (cons 3 '()). I had to add 3 to an empty list to make a list
; of one element. This is because a list had the head of the list, and the rest
; of the list. The rest of the list is either a list itself or the empty list.
; This can get confusing when we have these things called pairs
(cons 2 3)
`(2 . 3)
(cons 2 3) ; what is this?
; this is a pair. A pair of items. A list is basically a pair, except the second
; item is a list. Also we will get to the ' notation in the quoting section.
(first (cons 2 '())) ; returns 2
(rest (cons 2 '())) ; returns the empty list
(rest (cons 2 3)) ; is an error
; to get the head and tail of a pair, we use this thing called the car and cdr.
(car (cons 2 3)) ; returns 2
(cdr (cons 2 3)) ; returns 3
(cdr (cons 2 (cons 3 1))) ; returns '(2 .3)
(car (cons 2 '())) ; returns 2
(cdr (cons 2 '())) ; returns the empty list
(cdr (cons 2 (cons 3 '()))) ; returns '(3)
; notice that a pair is seperated by a '.' character. 
```

We may need to parse data out from a list, and we will see this in pattern
matching and unquoting.

### Matching

We use pattern matching for lists and pairs and seeing how in Racket, everything
can be treated as a list, this becomes really important.
The syntax is similar to Ocaml, but with more parenthesis and brackets.
```Racket
(define (product x)
	(match x
		['() 0]
		[(cons x '()) x]
		[(cons x y) (* x (product y))]))		
; we have a list where the function name is match, the first argument is what is
; going to be matched, and then the subsequent arguments are lists consisting of
; a pattern and code to be run. Brackets and Parenthesises can be used 
; interchangability.
(equal? '() '[])
; Think of it this way
; match(x, ('() 0), ((cons x '()) x), ...)
; each of the match lists consist of a pattern to match and code to run if the 
; pattern matches. 
; Additionally, we can use pattern matching to findgure out a type and bind it
(define (is_char? x)
	(match x
		[(? char? c) #t]
		[(? integer? i) #f]
		[_ #f]))
```

### Quoting

As you may have seen, I have been writing some lists with the `'` character.
These quotes help differentiate lists from Rackets normal structure and that's
because the `'` operator creates symbols. Anything can be made into a symbol
and when we make lists, we are making a list of symbols (at least in this class
as we will only be dealing with primitives). That is because quoting tells Racket
to treat code as data (and unquoting says to treat data as code).

```Racket
'(1 2 3) ; this is the same as (list 1 2 3)
(equal? '(1 2 3) (list 1 2 3)) ; returns #t
(equal? '(x y z) (list 'x 'y 'z)) ; returns #t
; The quote is just short hand notation of calling (list 'a 'b ...'n)
; This is how we can treat code as data. 
(+ 1 2) ; is the function application of + to the arugments 1 and 2.
'(+ 1 2) ; is the list of function name and parameters and the same as 
(list '+ '1 '2)
; However, suppose we need to treat some of these symbols as actual varaibles
; or even as a function application itself. This is where we introduce the
; concept of a quasiquote and unquoting. That is, treating code as data.
`(1 2 3) ; this is the quasiquote character. It acts the same as ', but has some
; additional functionality.
; Namely, we can unquote and treat some values not as symbols, but as code.
`(+ 1 ,(* 2 3)) ; we use the , character to unquote and treat the following
; expression as code and not as data.
`(+ 1 ,(* 2 3)) ;is the same as
(list '+ '1 (* 2 3)) ; where (* 2 3) is code and evaluates to 6.
(list '+ '1 6) ; and since primitives are the same as thier corresponding symbol,
(equal? (list '+ '1 '6) (list '+ 1 6))
```

Being able to convert data to code and code to data is what makes meta programming
so interesting. We can do this in Racket by mixing this quoting and unquoting
idea by pattern matching.

```Racket
; consider the following
(match  x
	[`(cons ,x ,y) (cons x y)])
```
This small segment of code is basically what I want to point out. I am matching
on a list that follows a pattern and can parse out data from it, then use these
parsed out values as data for expressions.

This is why I have been emphasizing that in Racket, everything is a list, and 
that all we need to do is just understand what these lists represent.
