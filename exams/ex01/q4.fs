\ remember, in forth you return flags. -1 is true and 0 is false
create input_list 100 cells allot
variable list_size 0 list_size !

( assume that list_end is initilaized to the size of the list )
( assume that input_list is initilaized and holds the list of ints )
( co1 is the coefficent for the a_{n-1} term in linear combination )
( co1 is the coefficent for the a_{n-2} term in linear combination )
( place 0 or -1 on the stack when complete )
: checkarray ( co1 co2 -- bool )
( your code here )

