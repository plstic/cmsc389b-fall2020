:- initialization run,halt.

run_f(Func) :- Func.
testcases([testWumpus, testPit,testPath]).

run :-
    testcases(TC),
    maplist(run_f,TC).

testWumpus :- 
        write('% testWumpus1: '),
        ( wumpus(3) -> Z = true ; Z = false),write(Z),nl.
testPit :- 
        write('% testPit1: '),
        ( pit(2) -> Z = true ; Z = false),write(Z),nl,
        write('% testPit2: '),
        ( pit(4) -> Z = true ; Z = false),write(Z),nl.
testPath :- 
        write('% testPath1: '),
        killWumpus(13,W),write(W),nl.

neighbor(1,2).
neighbor(1,5).
neighbor(1,8).
neighbor(2,3).
neighbor(2,10).
neighbor(3,4).
neighbor(3,12).
neighbor(4,5).
neighbor(4,14).
neighbor(5,6).
neighbor(6,7).
neighbor(6,15).
neighbor(7,8).
neighbor(7,17).
neighbor(8,9).
neighbor(9,10).
neighbor(9,18).
neighbor(10,11).
neighbor(11,12).
neighbor(11,19).
neighbor(12,13).
neighbor(13,20).
neighbor(13,14).
neighbor(14,15).
neighbor(15,16).
neighbor(16,17).
neighbor(16,20).
neighbor(17,18).
neighbor(18,19).
neighbor(19,20).
smell(2).
smell(4).
smell(12).
breeze(1).
breeze(3).
breeze(10).
breeze(5).
breeze(14).

