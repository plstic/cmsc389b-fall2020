# Exam 1

## Question 1

Why do multiple languages exist when you could write all of your code in one language?

## Question 2

Which type of logic does Prolog use:
- [] propositional logic
- [] predicate (first-order) logic
- [] second-order logic
- [] imaginary logic

## Question 3

Choose one of the following, and respond.

> How are security languages and concurrent languages related?

> How are array programming languages and concurrent languages related?

## Question 4

Using any language we have learned thus far, solve the following.

> Given an array of integers and a recursive sequence definition, determine whether the array elements align with the sequence definition.

Assumptions:
* the input sequence will always be a linear combination of the previous two terms
* the input array will always contain integers
* the input array will always contain at least two elements
* the sequence base-cases are determined by the first two elements in the array
* you will be given the coefficients for the linear combination

Examples:
```txt
a_n = a_{n-1} + a_{n-2}

in:  [1,1,2,3,5,8], 1, 1
out: true

in:  [1,1,2,3,4,8], 1, 1
out: false


a_n = 2*a_{n-1} - a_{n-2}

in:  [2,1,0,-1,-2,-3], 2, -1
out: true

in:  [2,2,0,-2,-4,-5], 2, -1
out: false
```

Your code should be syntactically correct.
Template code for each language appears in this repository.

## Question 5

Using any language -- **different than the one used in Q4** -- we have learned thus far, solve the following.

> Given a list of integers, remove all multiples of 5.

Assumptions:
* the input list will be non-empty
* the input list will always contain integers

Examples:
```txt
in:  [1, 2, 3, 4]
out: [1, 2, 3, 4]

in:  [5, 60, 6, 7, 8]
out: [6, 7, 8]

in:  [5]
out: []

in:  [-5, -10, -15, 20, 25, 30]
out: []
```

Your code should be syntactically correct.
Template code for each language appears in this repository.

## Bonus

What language feature is most important, and why?
What is the most important part of learning a language, and why?
Is _Cats_ (2019 film) actually a good movie?
