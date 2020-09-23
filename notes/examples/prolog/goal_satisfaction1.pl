parent(daphne,cyrus).
parent(astoria,cyrus).

sisters(X,Y) :- X \== Y,parent(X,Z),parent(Y,Z).
