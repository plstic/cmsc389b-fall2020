# Cards

Due: 11 October 2020, 11:59pm EDT  

Points: 25 Public, 75 semipublic

## Before You Start

Please make sure that if you are not using docker (or have updated things on the
docker image), that you are using the at least `gforth 0.7.3`. You 
can check this by running `gforth`. If the command does not run, then 
you do not have gforth. You must have gforth, as other implementation 
versions may cause tests to fail.

VS Code Extentsion: https://marketplace.visualstudio.com/items?itemName=fttx.language-forth
idk, I dont use VScode but this should work. 
### Introduction

You will be implementing a a simulation of operations that could occur to a decl
of cards, with 4 players and a dealer.

#### Testing and Submitting

You will submit this project to [gradescope](https://www.gradescope.com/courses/172268).
You may only submit the `cards.fs` file. To test locally, run 
`python run_tests`. If there is no output then you have passed all the tests.

### Cards

#### Properties of the deck

In this implementation, there will be 52 cards, labled  1 to 13. There will be 
no distinguishing suits, and no jokers. 

#### Implementation

I give you a varaible called deck which allots 52 cells.
This will be the main deck of the project, however you may want to allot
more variables for each of the 4 players to keep track of any hands they may 
hold.

I ask that you treat index 0 of the deck as the top of the deck and index 51
to be the bottm of the deck.

#### Deck Operations

 + `mkdeck` - this will initialize the intial deck, in new deck order. 
 + `print_deck` - this will print the deck from top to bottom.
 + `shuffle` - this will interweave the top 26 and bottom 26 cards into one.
 + `cut_deck` - this will split the deck in half and swap the halves.
 + `deal` - this will distribute the deck evenly between the 4 players. 
 + `return_to_deck` - will take the card off the top of the stack and place it on the bottom of the deck. Will never be called on an empty stack.
Except for `print_deck` and `return_to_deck`, all of these calls will only 
happen on a full deck.

#### Player opertions

	+ `placeCards` - Will take the first card given to each player and place them on the stack in the order of player 1, player 2, player 3, player 4. Will only be called if all players have at least one card.
	+ `p1draw` - will take the card at the top of the deck and place it in player one's hand.
	+ `p2draw` - will take the card at the top of the deck and place it in player tow's hand.
	+ `p3draw` - will take the card at the top of the deck and place it in player three's hand.
	+ `p4draw` - will take the card at the top of the deck and place it in player four's hand.

#### stack operations

	+ comp - will compare the top 4 values on the stack and push a number onto the stack denoting which values are the highest. 

#### Evalutation

+ 25% Public Tests
+ 75% Semipublic Tests

The code for the publis tests are given to you. When you submit to gradescope,
you will be given the names of the semiublic tests and told if you fail or pass
them.
