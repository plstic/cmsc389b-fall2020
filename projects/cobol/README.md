# COBOL

## Preliminaries

We will be using the `GnuCOBOL` (formerly [openCOBOL](https://sourceforge.net/projects/open-cobol/)) compiler.
We are using version `3.1-rc1.0`.
To check, you can run `cobc --version`:
```txt
[student@94348cfd8109 ~]$ cobc --version
cobc (GnuCOBOL) 3.1-rc1.0
Copyright (C) 2020 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
Written by Keisuke Nishida, Roger While, Ron Norman, Simon Sobisch, Edward Hart
Built     Jul 04 2020 16:02:21
Packaged  Jul 01 2020 00:39:38 UTC
C version "10.1.0"
```

This assignment is due **Friday, September 18th at 11:59pm on Gradescope**.
You will submit one `hello.cob` and one `besbubo.cob` to the `COBOL` Gradescope assignment.
You will be graded on public and private tests.

The [`Makefile`](Makefile) contains compilation and testing labels.
To run public tests, run `make test` (this includes compilation).
To just compile, run `make all` or `make`.

COBOL source code files end with `*.cob`.

### Documentation

* <https://www.tutorialspoint.com/cobol/index.htm> (good tutorial)
* <http://opencobol.add1tocobol.com/gnucobol/#do-you-know-any-good-jokes>

Refer to the lecture notes for more.

### VSCode

[Endorsed extension](https://marketplace.visualstudio.com/items?itemName=bitlang.cobol)

## Assignment

### Hello, world!

Your first task is to edit `hello.cob` and have it output: `Hello, world!`.

### Besbubo

Onto the main project.
Your code for this will all be placed in `besbubo.cob`.

In this project, we're making `besbubo` -- a sort-of Testudo clone.
`besbubo` is a tool that allows students to enter in their student schedules, and output said schedule in pretty ascii format.
If we really wanted to re-create Testudo, we could actually use the SQL database features included with COBOL for a persistant data-store.
We will be learning SQL later, though, so we can just store our single-person session in-memory :)
This assignment is for _procedural_ programming, so let's keep it that way.

There are two ways to run `besbubo` -- using the interactive shell (CLI -- command-line interface), or as a command-line utility:
```txt
# interactive shell
[student ~] ./besbubo -i
Welcome to Besbubo
Type '\h' for help
besbubo> \h
 \h -- displays help
 \q -- quits shell
 \p -- prints current schedule
 \c -- clears current schedule
 \a [...] -- adds provided course
besbubo> \a CMSC250 01010 1100
 - added CMSC250 01010 1100
besbubo> \q
Bye

# command-line utility
[student ~] ./besbubo < commands.txt > result.txt
[student ~] cat result.txt
...
```

Now, we are simplifying schedules quite a bit in this project.
All classes are assumed to be one hour, starting on the hour or at the half hour, and during the time frame `0800-1700` (8am to 5pm) (not inclusive of 5pm).
We don't care about discussion sessions or complicated lab sessions.
Classes are entered in the following format:
```txt
XXXXYYY ##### $$$$

X = 4-letter school code
Y = 3-number course code
# = Monday-Friday day indicator (0=No, 1=Yes)
$ = 24-hour format time (0000-2359)
```
For example, we might represent _this_ course as:
```txt
CMSC389 00001 1100
```
We also don't care about if the school code or course code are valid.
For example, `ABCD999 11111 1230` is perfectly fine.

All of our tests will explicitly follow the above format -- **you do not need to handle malformed input**.
For example, your code need not handle:
```
C M S C2 50     test 000
 CS9C250	02020 1$$0
```

Schedule printing looks roughly like this:
```txt
besbubo> \a CMSC389 00001 1100
besbubo> \a MATH401 10101 1000
besbubo> \p
     |    M    |    T    |    W    |    T    |    F    
-----+---------+---------+---------+---------+---------
0800 |         |         |         |         |         
0830 |         |         |         |         |         
0900 |         |         |         |         |         
0930 |         |         |         |         |         
1000 | MATH401 |         | MATH401 |         | MATH401 
1030 | MATH401 |         | MATH401 |         | MATH401 
1100 |         |         |         |         | CMSC389 
1130 |         |         |         |         | CMSC389 
... <for brevity; you would print every time slot> ... 
1600 |         |         |         |         |         
1630 |         |         |         |         |         
-----+---------+---------+---------+---------+---------
besbubo> \q
Bye
```

`besbubo` does _mild_ error checking:
* ensuring classes do not overlap
* ensuring classes start only on the hour or half-hour

**Note**: you may have to do some tricky processing with times.
Remember, the half-hour mark is at `00:30`, not `00:50`.
This can get confusing since times may be stored as integers, so we might represent `00:30` as literal `0030` (which is not half of 100).

### Implementation

There are a few key parts to this system that _you_ have to implement.
The boilerplate code is provided, as well as all variables we used when creating the project.
Feel free to use the provided `WORKING-STORAGE SECTION.` variables, or completely rip it out and use your own.
Fair warning: the provided code likely depends on some (or all) of the given variables, so take caution when refactoring.
The rest of this spec will assume you are using most of the given variables.

Note that we are testing your implementation as a **black box**.
You can implement your code however you like, so long as it outputs the correct/expected information/data.
Keep in mind, though, that to make the project easier we have given you most of the starter code.
As such, there's some assumption in this spec that you'll be using said starter code.

We have four different tasks for you to implement.
* quitting
* inserting
* printing
* clearing

These are in order of appearance in the project, but you can implement them in any order.
It might be difficult to test your code without having implemented printing, though.

#### Quitting

Ah, this one should be straightforward.
Your code should quit whenever a `\q` is emitted to the program.
A switch-case is already provided to look for `\q` inputs, and it calls the `set-break-stop` procedure.
All you have to do in this procedure is set the working-storage variable `WS-BREAK` equal to 0.
This will cause the sentence `PERFORM console-loop UNTIL WS-BREAK = 0.` to break out of the main loop and exit the program.

#### Inserting

Inserting classes can be tricky.
Remember that **classes are always one hour**, and no classes occur before 8am (0800) or after 4:59pm (1659).
A user will type into the terminal `\a XXXXYYY ##### $$$$` (for example, `\a MATH401 10101 1030`), and the class should be added to the internal schedule.
Your code should do the following, in-order:
1. check that the given time `$$$$` is valid
   1. verify that the time is at-least 8am and at-most 4pm
   1. verify that the time falls on the hour (`##00`) or on the half-hour (`##30`)
1. check that there are no classes already scheduled during the inputted hour
   * you must ensure that both half-hour blocks do not already contain a class

If the first step fails, `WS-SUCCESS` must contain the integer 2.
If the second step fails, `WS-SUCCESS` must contain the integer 1.
Yes.

If both steps succeed, then the class should be inserted into your internal schedule for both half-hour blocks `$$$$` and `$$$$ + 30min`.
Then, `WS-SUCCESS` should contain the integer 0.

> Oh great instructors, how do we translate the integer time `$$$$` to a usable index into, say, a schedule table?

We are glad you asked.
This class is not an algorithms course, so we sketch the idea here:
```txt
idx <- $$$$ (input) (ex:)
idx = idx + 20      ( 900 ->  920)
idx = idx * 2       ( 920 -> 1840)
idx = idx - 1600    (1840 ->  240)
idx = idx / 100     ( 240 ->    2)
idx = idx + 1       (   2 ->    3)

maps [800, 830, 900, 930, ...]
  -> [  1,   2,   3,   4, ...]
```
You certainly don't have to use this!
You may find some form of reversal of this procedure useful for displaying the time during printing (up next).

#### Printing

Printing is the most important part of this ordeal.
Your code should "pretty print" its internal schedule whenever a `\p` is emitted to the program.

An empty schedule should look _exactly_ like this:
```txt
     |    M    |    T    |    W    |    T    |    F    
-----+---------+---------+---------+---------+---------
0800 |         |         |         |         |         
0830 |         |         |         |         |         
0900 |         |         |         |         |         
0930 |         |         |         |         |         
1000 |         |         |         |         |         
1030 |         |         |         |         |         
1100 |         |         |         |         |         
1130 |         |         |         |         |         
1200 |         |         |         |         |         
1230 |         |         |         |         |         
1300 |         |         |         |         |         
1330 |         |         |         |         |         
1400 |         |         |         |         |         
1430 |         |         |         |         |         
1500 |         |         |         |         |         
1530 |         |         |         |         |         
1600 |         |         |         |         |         
1630 |         |         |         |         |         
-----+---------+---------+---------+---------+---------
```
If a time-period contains a class, it should be displayed in the correct cell.
Cells are 9 characters long, except the first column that displays the time.
Empty cells should be 9 spaces, and filled cells should be the full class name padded with a space on each side.
Empty: `.........` (spaces instead of periods), Occupied: `.CMSC420.` (again, spaces instead of periods).
Don't forget -- the last column should also have proper whitespace!
If your output looks correct, but you're failing a test, make sure you're properly spacing your prints!

A schedule with:
```txt
\a CMSC389 00001 1100
\a MATH401 10101 1000
```
should look like:
```txt
     |    M    |    T    |    W    |    T    |    F    
-----+---------+---------+---------+---------+---------
0800 |         |         |         |         |         
0830 |         |         |         |         |         
0900 |         |         |         |         |         
0930 |         |         |         |         |         
1000 | MATH401 |         | MATH401 |         | MATH401 
1030 | MATH401 |         | MATH401 |         | MATH401 
1100 |         |         |         |         | CMSC389 
1130 |         |         |         |         | CMSC389 
1200 |         |         |         |         |         
1230 |         |         |         |         |         
1300 |         |         |         |         |         
1330 |         |         |         |         |         
1400 |         |         |         |         |         
1430 |         |         |         |         |         
1500 |         |         |         |         |         
1530 |         |         |         |         |         
1600 |         |         |         |         |         
1630 |         |         |         |         |         
-----+---------+---------+---------+---------+---------
```

#### Clearing

Your code should clear its internal schedule data whenever a `\c` is emitted.
To do this, you should fully reset the variables that store displayed information.
The schedule should print as **empty** after calling a clear.

### Evaluation

* 20% Public Tests
* 75% Private Tests
* 5% style

The public tests should be self-explanatory.
`make test` should produce a nice `output.txt` file containing your test results.
It should state that you have passed each test.

The private tests can be seen after submission on Gradescope.
You will be able to see the names of each private test and whether you passed.

For full credit on this assignment, you must submit code that stylistically represents COBOL.
Look, you don't have to go overboard (feel free to `>>SOURCE FORMAT FREE`).
Capitalize your gosh-dang COBOL keywords, make good use of indentation, make your code read-able, etc.
Not only should it look like good COBOL, it should feel procedural.
This assignment corresponds to the procedural programming paradigm, and as such should make good use of procedures.
This is what we mainly care about -- that you can separate your procedural code into proper procedural steps.
