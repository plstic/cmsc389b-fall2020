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
