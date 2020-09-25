# Hunt the Wunpus

Due: 2 October 2020, 11:59pm EDT  

Points: 35 Public, 65 semipublic

## Before You Start

Please make sure that if you are not using docker (or have updated things on the
docker image), that you are using the at least `SWI-Prolog version 8.2.1`. You 
can check this by running `swipl --version`. If the command does not run, then 
you do not have swi-prolog. You must have swi-prolog, as other implementation 
versions may cause tests to fail.

### Introduction

[Hunt the Wunpus](https://en.wikipedia.org/wiki/Hunt_the_Wumpus) is a 1973 game
by Gregory Yob. I have simplified the rules and so you will be making an a very
rudamentary AI which will solve a variation of the game. 

#### Testing and Submitting

You will submit this project to [gradescope](https://www.gradescope.com/courses/172268).
You may only submit the `wumpus.pl` and `hello.pl` files. To test locally, run 
`make tests`. If there is no output then you have passed all the tests.

### Hello World 

Before you can begin, we ask you to write the "Hello, World!" program in prolog.
You just need to finish the `hello.pl` file. Should be easy enough.

### Wumpus

#### Wumpus Game PLay

The gameboard consists of 20 caves arranged around a dodecahedron. Each cave has 
3 neighbors which you can move to. The numbering of the caves is shown below:

![images/2d.png](/images/2d.png) ![images/3d.png](/images/3d.png)

There is one cave which houses the wumpus. There are 2 caves which have spiked
pits. If you go into a cave with a pit, you fall to your death. In this 
simplified version of the game, you are to just find a path to the wumpus given
a starting cave.

You know there is a pit in an adjacent cave because you will "feel a breeze".
YOu know there is a wumpus in an adjacent cave because you will "smell" it.

That is, assume the wumpus is in cave 3. YOu will smell the wumpus in caves 
2, 4, and 12. Assume there are 2 pits, one in cave 2, and one in cave 4. You 
will feel a breeze in caves 1, 3, 5, 10, and 14. Thus, if you start in cave 17,
you should find the path `[17,18,19,11,12]`.

#### Implementation

You will be given some of the game rules along with 3 helper rules:

+ Game rules
	+ `neightbor(X,Y)` - `true` if X and Y are neighboring caves
	+ `smell(X)` - `true` if you smell the wumpus in cave X
	+ `breeze(X)` - `true` if you feel a breeze in cave X
+ helper rules
	+ mklst(X,Y) - `true` if Y = [X,X-1,X-2,...3,2,1]
	+ notin(X,Y) - `true` if X is not in the list Y
	+ rev(X,Y) - `true` if list Y's elements are in reverse order of the X's 
	elements

You will need to implemet two procedures: `wumpus(X)` and `killWumpus(Y,W)`.
The `wumpus(X)` rule will return true if X is the cave with the Wumpus.
The `killWumpus(Y,W)` rule will return true if W is 
the shortest path of caves numbers starting at Y, and ending in the Wumpus' 
cave. 

#### Evalutation

+ 35% Public Tests
+ 65% Semipublic Tests

The code for the publis tests are given to you. When you submit to gradescope,
you will be given the names of the semiublic tests and told if you fail or pass
them.
