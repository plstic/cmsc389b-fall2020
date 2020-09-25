% given functions

% mklst(5,Z) will return Z = [5,4,3,2,1]
% mklst(X,[5,4,3,2,1]) will return X = 5.
% mklst(5,[5,4,3,2,1]) will return true
% mklst(4,[5,4,3,2,1]) will return false
mklst(0,[]).
mklst(X,[X|Y]) :- X1 is X - 1,mklst(X1,Y).

% notin(5,[5,4,3,2,1]) will return true
% notin(4,[5,4,3,2,1]) will return false
notin(X,[]).
notin(X,[H|T]) :- X \== H, notin(X,T).

%rev([5,4,3,2,1],X) will return X = [1,2,3,4,5]
%rev(X,[5,4,3,2,1]) will return X = [1,2,3,4,5]
rev([],[]).
rev([H|T],L) :- rev(T,Z), append(Z,[H],L).


% add some helper functions if needed/wanted


% wumpus(X) returns true if the wumpus is at X
wumpus(X) :- .% finish this rule


% killWumpus(1,W) will return a list of [1,X1,X2,...,Xn] where X1,X2,...Xn are 
% the nodes from 1 to the Wumpus as Xn.
% These nodes in a list should represent the shortest path from th starting 
% point Y, to the Wumpus.
killWumpus(Y,W) :- %finish this rule
