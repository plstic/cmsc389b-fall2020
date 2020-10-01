\ built in

1 2 .s
swap .s
nip .s
drop .s
1 2 * . .s
3 4 * 6 + 7 5 - + .s


\ sample program 
  : cube ( n -- n^3 ) dup dup * * . ;
  : as ( n1 n2-- n3 ) over over - rot 1 + * . drop ; 
  : as2 ( n1 n2-- n3 ) 2dup - rot 1 + * . drop ; 

\ local variables for colon definitions
  : 3rot { a b c -- c a b }  c b a ;

\ looping
  \ while
  : whileEx ( n -- n' )
  BEGIN
    dup 
    dup .
    0>    
  WHILE
    1- 
  REPEAT drop ;
  \ do while
  : doWhileEx ( n -- n' )
  BEGIN
    1-
    dup .
    dup 0=
  UNTIL drop ;

  \ for 
  : forEx ( -- )
  10 0 ?DO
    i 2 * .
  LOOP ;

\ recursion 

  \ non-standard
  : fac0 ( n -- n! ) recursive
    dup 0> if dup 1- fac0 * else drop 1 endif ;

  \ standard I guess
  : fac1 ( n -- n! ) 
    dup 0> if dup 1- recurse * else drop 1 endif ;

  \ mutual recursion
  Defer isEven

  : isOdd ( n - n ) dup 0 <= if drop -1 else 1 - isEven endif ;

  :noname ( n - n ) dup 0 <= if drop 0 else 1 - isOdd endif ; IS isEven

\ memory
  \ global varaible
  variable g
  \ store value
  1 g !
  \ fetch value
  g @ .

  \ arrays/heap continous memory
  create arr 5 cells allot
  10 arr 0 cells + !
  arr 0 cells + @ . ( many versions of this as it uses pointer arrith )

\ characters and strings 
  create str 104 c, 101 c, 108 c, 108 c, 111 c,
  create str2 5 cells allot
  119 str2 !
  111 str2 1 chars + !
  114  str2 2 chars + !
  108 str2 3 chars + !
  100 str2 4 chars + !
  str 5 type 
  str2 5 chars dump
