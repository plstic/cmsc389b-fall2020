NB. print the input
display =: (1!:2) & 2

NB. funcs/args are applied right-to-left

NB. the best way to figure out what's going on
NB. is to break each statement into each minimal part
NB. and execute that part by itself (in jconsole)
NB.  see what output is being mapped from the input

display ''
display '--- BASICS ---'
display ''

NB. basics
s =: 5
a =: 1 2 3 4 5
b =: i.5
c =: |. b NB. reverse the list
d =: < a NB. box up a

NB. u&v y  (monad form) returns  u v y
NB. so it applys v to every element of y
NB. then applys u to every element of v y
NB. so if we want to do a single function, we just let u or v be the identity function [ or ]
NB.  then ]&display will "display" every input element
NB.  a;b;c boxes up each list
NB.  and > unboxes them into a 2d array
NB.  finally, our ]&display will "display" each row of the array!
]&display >(s;a;b;c;d)
NB. notice the scalar s gets padded with zeros, and the box d gets unboxed
NB. this is just what happens from the box concatenation ;

NB. these lines just display the values again
display s
display a
display b
display c
display d

NB. some computations
NB. make sure your array inputs have the same length!
display a+b
display c%a
display a%c NB. note the div-by-zero yields _ (infinity, null negative)
display a|c
display c/:c  NB. sort c
display /:~~c NB. same as above

display ''
display '--- MANIPULATIONS ---'
display ''

NB. shape yields a list of dimensions
display $ a
NB. rank yields number of dimensions
NB.  this is just the length of a list
display # $ a
NB. see:
display # a NB. (returns length of a)
NB. reshape puts data into array of left-arg dimension
display 2 2 $ 1 0 0 1
display 3 3 $ 1 0 0 0 1 0 0 0 1 2
display 3 3 $ 2 0 0 0 1 0 0
display 3 3 $ i. 9 NB. i. takes right-input and returns list [0 <= array indices < input]
NB. copy does:
NB. Creates a new array in which each integer in x controls how many times the corresponding item of y appears
display 1 2 3 # 'abc'
NB. -> abbccc
NB. If x is an atom (a scalar value) then it controls every item of y
display 3 # 'abc'
NB. -> aaabbbccc

display ''
display '--- FUNCS ---'
display ''

NB. control flow is done inside functions
NB. functions can be in-line or explicit
NB. functions can be monadic (one arg) or dyadic (two args)
NB.  monadic has right-argument y
NB.  dyadic has left-argument x and right-argument y
NB. the last line of any function is always what's returned

NB. these two are the same
func1 =: 3 : 'y'
func2 =: 3 : 0
  y
)
display func1 'hi'
display func2 'hi'
NB. these two are the same
func3 =: 4 : 'x+y'
func4 =: 4 : 0
  x+y
)
display 2 func3 3
display 2 func4 3
NB. 'a' func3 'b'  yields domain error b/c you can't add strings
NB. you could replace + with , for concatenation though!

func5 =: 3 : 0
NB. <: is less-than-or-equal
  if. 4<:y do. '4 <= arg'
  else. 400
  end.
)
display func5 4
display func5 3
NB. array ex of <:
display a<:c NB. does comparisons element-wise

NB. if you need to use a string in an in-line func:
funcquot1 =: 3 : 'if. 0=y do. ''hello!'' else. 1 end.'
funcquot2 =: 3 : 0
  if. 0=y do. 'hello!'
  else. 1
  end.
)
display funcquot1 0
display funcquot1 1
display funcquot2 0
display funcquot2 1

display ''
display '--- LOOPS ---'
display ''

NB. for-loops!
NB. note that  smoutput  is essentially the same as  display
func6 =: 4 : 0
  NB. do loop 5 times (#i.5 = 5)
  for. i.5 do.
    smoutput x;'repeat me';y
  end.
  NB. if you need the values/indices over a list
  for_i. 0 4 0 5 4 do.
    smoutput i;i_index
  end.
  for_changeme. |. 0 4 0 5 4 do.
    smoutput changeme;changeme_index
  end.
  '' NB. return nothing
)
display 'dank' func6 'memes'

NB. while loops
func7 =: 3 : 0
  i =: 0
  idx =: i.y
  while. i<#idx do. NB. while the statement is true, do the body
    smoutput i { idx
    i =: i+1
  end.
  NB. whilst is a do-while
  NB.
  NB. whilst. T do. B end.
  NB.  <=>
  NB. B
  NB. while. T do. B end.
  i =: 0
  whilst. i<#idx do.
    smoutput i
    i =: i+1
  end.
  'something random'
)
display func7 5

display ''
display '--- SCOPING ---'
display ''

NB. care about scoping?
NB. https://www.jsoftware.com/help/learning/12.htm
L =: 'old L'
G =: 'old G'
foo =: 3 : 0
  L =.  y NB. local variable L
  G =:  y
  L
)
display foo 'new'
display L
display G

NB. uncomment to _leave_ the jconsole at the end of script
NB. exit 0
