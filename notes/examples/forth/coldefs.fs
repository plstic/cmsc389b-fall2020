\ sample program 
  : cube ( n -- n^3 ) dup dup * * . ;
  : as ( n1 n2-- n3 ) over over - rot 1 + * . drop ; 
  : as2 ( n1 n2-- n3 ) 2dup - rot 1 + * . drop ; 

\ local variables for colon definitions
  : 3rot { a b c -- c a b }  c b a ;

