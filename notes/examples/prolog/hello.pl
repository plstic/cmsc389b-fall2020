:- initialization main, halt.

list_length([],0).
list_length([_|Xs],T+1) :- list_length(Xs,T).

main :-
  list_length([1,2,3],0+1+1+1),
	write('Hello World'), nl.
