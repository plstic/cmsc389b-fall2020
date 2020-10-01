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
