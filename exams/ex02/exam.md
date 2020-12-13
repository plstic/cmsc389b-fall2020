# Exam 2

## Question 1

How are declarative languages and APIs similar?
How are they different?

## Question 2

In what cases are using esoteric languages like INTERCAL or Befunge appropriate?
If there are none, justify.

## Question 3

What term did we use to refer to language-independent concepts -- patterns that transcend programming languages?
Mark the correct answer, and provide an example.
- [] bahuvrihi
- [] collocation
- [] (programming) idioms
- [] pizza

## Question 4

Choose one of the following:
1. Take the racket compiler from lecture and add two functions:
   + `(sq expr)` -> which will square the expression
   + `(even? expr)` -> will return 1 if expr is even or 0 if expr is odd.
2. Take the flex-calc example from lecture and add two operations:
   + `[expr]` -> squares the expression
   + `{expr}` -> returns 1 if the expr is even or 0 if the expr is odd

The code for racket is available in the `racket` folder.
The code for the flex-calc is in the `fb` folder.

## Question 5

Either in INTERCAL or Befunge (choose whichever you prefer), write a program that will square an integer.
The input is a single integer, read-in by the program.
The output is that integer squared, printed to standard-out.

Then, repeat for sqlite.
Assume you are given a database `input.db` generated by:
```sql
CREATE TABLE input ( val INTEGER );
INSERT INTO  input ( val ) VALUES ( 420 ); -- 420, or replace with any integer from the "input"
```
Write a sqlite statement that squares this "input".
Assume your statement is written in a file like `q5.sql` and is executed like `sqlite3 input.db < q5.sql`.

Within the entire concept of "squaring an integer", which program, INTERCAL/Befunge or sqlite, was harder to implement?
Provide a complete justification.
Note that we care holistically about the **entire process**, not _just_ the specific code you wrote for each language.

## Bonus
Do you pronouce 'SQL' as 'S-Q-L' or 'Sequel'?
Which is correct, and why?