create deck 52 cells allot

[                                            ]
2 deck 0 cells + !


\ so you probably want to also initial hands for 4 people
\ and make some varaibles to keep track of card positions
\ how you do this and the names of varaibles should not matter
 
\ initialize the deck array to hold 52 cards, no suits
\ in new deck order. That is, 
\ [1,2,..,12,13,1,2,...,12,13,13,12,...,2,1,13,12,...,2,1]
\ top of the deck is index 0. bottom of the deck is index 51.
: mkdeck ( -- )
\ your code
; 

\ print out the deck starting from the top of the deck 
\ to the botom. Top is index 0 of deck and bottom is index
\ 51 of deck. Stop when no more cards are in the deck.
: print_deck ( -- )

;

\ take the top 26 cards of the deck array and place them at the even
\ indicies and take the last 26 cards of the deck array and place them
\ at the odd indicies. Scaled down to a deck of 6 cards, it would look 
\ like ( [1 2 3 4 5 6] -- [1 3 2 4 5 6] )
\ will only be called on a full deck
: shuffle ( -- )

;

\ take the top 26 cards swap with the last 26 cards. Scaled 
\ down to a deck of 6 cards should look something like
\ [1 2 3 4 5 6] -- [4 5 6 1 2 3]
\ will only be called on a full deck
: cut_deck ( -- )

;

\ starting from the top of the deck, give player one a card, 
\ then player two, then player three, then player four. Do so until
\ the deck is empty. Will only be called on a full deck
\ [1 2 3 4 5 6 7 8] -- H1 = [1 5], H2 = [2,6], H3 = [3,7], H4 = [4,8]
\ order should not matter for the player's hand
: deal ( -- )

;
\ have each player place the first drawn card onto the forth stack
\ For eample, suppose we have a deck of 8 cards:
\ [1 2 3 4 5 6 7 8]. If we call deal, then each players hand should
\ look like H1 = [1 5], H2 = [2,6], H3 = [3,7], H4 = [4,8]. The internal
\ order should not matter, but the first card H1 drew was 1.
\ Thus if I call 'placeCards .s' I should see <4> 1 2 3 4
: placeCards ( -- ) 
\ your code
;

\ takes the card at the top of the deck and places it 
\ into player one's hand. 
: p1draw ( -- )
\ your code
;

\ takes the card at the top of the deck and places it 
\ into player two's hand. 
: p2draw ( -- )
\ your code
;

\ takes the card at the top of the deck and places it 
\ into player three's hand. 
: p3draw ( -- )
\ your code
;

\ takes the card at the top of the deck and places it 
\ into player four's hand. 
: p4draw ( -- )
\ your code
;

\ will take the top card on the forth stack and place it at bottom
\ of the deck. '.s <4> 1 2 3 4 return_to_deck .s <3> 1 2 3'
\ if the deck is empty, then return_to_deck is called, then the
\ deck would have one card, 4.  Will never be called on an empty stack
: return_to_deck ( n1 -- ) 
\ your code
;

\ take the top 4 cards of the stack and return  number indicating 
\ which card(s) is the highest. 
\ If you think about the top 4 cards like so:
\ [1 2 3 4], make a coresponsing array marking the highest card
\ [0 0 0 1]. Treat this array as binary and convert to decimal.
\ 0001 -> 1. Other examples include
\ [4 4 2 1] -> 12, [4 1 4 2] -> 10, [2 1 3 3] -> 3.
: comp { a b c d -- a b c d n }
\ your code
;
