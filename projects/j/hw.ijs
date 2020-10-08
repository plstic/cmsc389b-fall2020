NB. preliminaries

NB. comments start with "NB." (it has to be NB., it can't be: NB, nB., Nb., nb., etc)
NB. functions are either monadic or diadic -- one-arg or two-arg
NB. there are multiple ways to establish a function
NB. the left-arg is x, and the right-arg is y
NB. J is essentially a scripting language (as are most array-oriented languages)
NB.  -> just like Ruby, the last line in a function is what gets returned

NB. in our tests, we will assume you are using the standard profile.ijs
NB. (this is imported by default, so you don't have to do anything)
NB. if you want to see what's being loaded, you can run
NB.    >4!:3''
NB. this loads, in your docker container, /etc/j/9.01/profile.ijs
NB. as well as some other system scripts

NB. code time

NB. hello should store the string "Hello, World!"
NB. such that calling  hello  will yield the above string
NB. no arguments allowed
hello =: ''

NB. perform multiplication
NB. return left argument multiplied by right argument
mult =: 4 : '0' NB. "4" establishes this as a dyadic function

NB. perform exponentiation (raise to power)
NB. return left argument raised to the power of right argument
pow =: 4 : '0'

NB. we can define functions as "scripts" too
NB. these are called "explicitly-defined" or "explicit" functions
NB. (this one is given to you ;)
Celsius =: 3 : 0 NB. "3" establishes this as a monadic function
    t =. y - 32 NB. =. establishes local variables, but =: establishes global variables
    t * 5 % 9
)

NB. J booleans act like C booleans -- 0 is false, anything else is true
NB. the following example showcases this:
bool =: 3 : 0
    if. y do. 'true'
    else. 'false'
    end.
)

NB. write a monadic function (right-arg) that returns 1 if the input y is 1
NB. and returns 0 otherwise
BackwardsBoolean =: 3 : 0
    _1
)

NB. now write a monadic function that prints
NB.  'true'  if the input is 1
NB.  'false' otherwise (C-standard booleans)
NB. you should use the previous function to solve this
tf =: 3 : 0
    ''
)

NB. write a monadic function that prints
NB. the y-th fibonacci number
NB. (<=0)->0, 1->1, 2->1, 3->2, ...
NB. solve this function using recursion
fibonacci =: 3 : 0
    _1
)

NB. perform the same fibonacci computation without recursion, so we can utilize the "array"-oriented-ness
NB. let's transform the fibonacci sequence into a matrix
NB.  f(n) = f(n-1) + f(n-2); f(1) = f(2) = 1
NB. consider the matrix eqn:
NB.  |a b| * |f(n-1)| = |f(n)  |
NB.  |c d|   |f(n-2)|   |f(n-1)|
NB. =>
NB.  a*f(n-1) + b*f(n-2) = f(n)
NB.  c*f(n-1) + d*f(n-2) = f(n-1)
NB. =>
NB.  f(n) = f(n-1) + f(n-2) = a*f(n-1) + b*f(n-2) so a = b = 1
NB.  f(n-1) = c*f(n-1) + d*f(n-2) so c = 1 and d = 0 work
NB. => our matrix is:
NB.   |1 1| = 2 2 $ 1 1 1 0
NB.   |1 0|
NB. then, just keep applying the resulting vector to that matrix (starting with (1 1))
NB.  so f(3) yields
NB. |1 1||1 1||1 1| * |1| = |5|
NB. |1 0||1 0||1 0|   |1|   |3|
NB. keen observers may have noticed that the initial vector is not even needed ;)
NB.  examine the sequence of matrices M^1 M^2 M^3 ...
NB. |1 1| |2 1| |3 2| |5 3|
NB. |1 0|,|1 1|,|2 1|,|3 2|,... (look at the anti-diagonal)
NB.
dot =: +/ .* NB. A dot B computes the matrix product AB
NB.
NB. algorithm:
NB.  y<1 then return 0
NB.  otherwise, create a 3-dimensional array of shape  y,2,2
NB.    so each element is the 2x2 fibonacci matrix
NB.   then *apply* matrix-mult (use  dot  defined above)
NB.    between each element
NB.  return the value on the anti-diagonal (either value, doesn't matter)
fibIter =: 3 : 0
    _1
)

NB. write a monadic function (right-arg) that says
NB.  1 if y is prime
NB.  0 otherwise (composite, 0, 1, or negative)
NB. we will do this by trial division.
NB. algorithm:
NB.  nix values less than 2 (return 0)
NB.  for each integer 2 <= i <= sqrt(y)
NB.   test if i divides y (y = 0 mod i)
NB.    if yes, return 0
NB.  return 1
prime =: 3 : 0
    _1
)

NB. assume right-arg y is a list
NB. return the head -- the first element
head =: 3 : '_1'

NB. assume right-arg y is a list
NB. return the tail -- the list without the first element
tail =: 3 : '_1'

NB. write a dyadic function that "zips" together x and y
NB.  assume x and y have the same length
NB.  assume x and y hold the same type
NB. eg: 3 4 5 7 zip 3 5 7 8  -> 
NB.  3 3
NB.  4 5
NB.  5 7
NB.  7 8
NB.
zip =: 4 : 0
    _1
)

NB. well, what if i want x and y to be different types?
NB. you'll notice that 1 2 3 zip 'abc' will not work :(
NB. BUT, if we _box_ each value, then it will work
NB. so, do the previous function but box the values instead.
NB. eg: 1 2 3 4 zipBox 'abcd' ->
NB.  ┌─┬─┐
NB.  │1│a│
NB.  ├─┼─┤
NB.  │2│b│
NB.  ├─┼─┤
NB.  │3│c│
NB.  ├─┼─┤
NB.  │4│d│
NB.  └─┴─┘
NB.
zipBox =: 4 : 0
    _1
)

NB. sorting is cool, so let's implement it.
NB.  now,  /:y  returns the positions that each element in y _should_ be in (ascending order)
NB.  so we could do something like  (/:y) { y  and we will have sorted y
NB. we're going to _write_ a sort, though!
NB.  specifically, we're writing mergesort (Justin's favorite sort)
NB. we first write merge, which combines the left-arg array x with the right-arg array y
NB. by taking the smaller head of each list and recursively merging that with the rest
NB.  assume x and y are lists of the same comparable type
merge =: 4 : 0
    _1
)
NB. use merge to write mergesort, which sorts in ascending order the right-arg array
NB. you might find it useful to write helper functions "lower" and "upper" which
NB.   split the input array in half (roughly)
mergesort =: 3 : 0
    y
)

NB. one last problem
NB.  let's implement a parenthesis-level viewer
NB. this problem is good to showcase array thinking
NB.  which, to be fair, we've already done
NB. ANYWAY
NB. we're creating a list of integers where each index's value represents the corresponding
NB.  input string character's parenthesis depth level
NB. eg: (hello (lambda () acc u))
NB.     1111111222222223223222210 (but as an array of integers)
NB. algorithm:
NB.  (input right-arg y, which is a list of characters)
NB.  first get the index-of each left and right parenthesis
NB.   this should be a list of integers where positions containing:
NB.     (  yields a 0
NB.     )  yields a 1
NB.  then, apply a translation to the resulting array
NB.     0 -> 1
NB.     1 -> _1
NB.     2 -> 0
NB.   you can do this by using x{y -- select the item with index x from the array y
NB.    (using x as an array should work)
NB.  finally, successively sum the array
NB.   eg: 1 0 _1 0 1 -> 1 1 0 0 1
NB.   you'll find the infix adverb  \  useful here
NB. NOTE: this should be one line. (you may need to double-quote something inside, just put '' instead of ')
parenthesis =: 3 : '_1'
NB. then, very nicely, if you're a compiler you can check that the last element is =0
NB.  and boom you have a parenthesis nesting checker.

NB. that's it for J
NB. array-oriented thinking is somewhat akin to functional language thinking
NB. it's hard.
NB.
NB. if you like data structures, you might think about how you could implement a graph.
NB. you could do a vertex list and edge list:
NB.  V = (some list of integers, representing nodes)
NB.  E = (some list of pairs of integers, each pair representing an edge)
NB. then you might consider how to write DFS or BFS on graphs like that
NB.  what would the input look like?
NB. interested in some extra credit? go ahead and write a breadth-first search for me :)
NB.  just message us with your solution and we'll hang it up on our fridge
