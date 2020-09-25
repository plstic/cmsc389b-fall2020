:- initialization run,halt.

run_f(Func) :- Func.
testcases([testHello]).

run :-
    testcases(TC),
    maplist(run_f,TC).

testHello:- 
        write('% testHello: '),
        hello_world.
