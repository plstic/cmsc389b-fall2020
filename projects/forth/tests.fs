: test_deal
  13 0 ?DO placeCards LOOP
  52 0 ?DO . LOOP ;
: test_setup
  mkdeck shuffle shuffle shuffle shuffle shuffle shuffle shuffle ;

mkdeck
s" test_mkdeck: " type print_deck 10 EMIT
shuffle 
s" test_shuffle: " type print_deck 10 EMIT
test_setup
deal
s" test_suhffle/deal: " type test_deal 10 EMIT

s" test_comp1: " type 
1 2 3 4 comp .
drop drop drop drop
1 2 4 3 comp .
drop drop drop drop
4 3 2 1 comp .
drop drop drop drop
1 4 3 2 comp .
drop drop drop drop
10 EMIT
