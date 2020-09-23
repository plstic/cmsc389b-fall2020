mklst(0,[]).
mklst(X,[H|T]) :- X1 is X - 1, mklst(X1,T).

notin(X,[]).
notin(X,[H|T]) :- X \== H, notin(X,T).

rev([],[]).
rev([H|T],L) :- rev(T,Z), append(Z,[H],L).

list_length([],0).
list_length([H|T],L) :- list_length(T,L1), L is L1+1.

factorial(0,1).
factorial(X,Y) :- factorial(X1,Y1), X is X1 + 1, Y is Y1 * X.

loves(mary,food).
loves(john,food).
loves(mary,books).
loves(mary,movies).
loves(john,mary).

