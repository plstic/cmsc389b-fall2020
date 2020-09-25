CC = swipl
PFLAGS = --goal=main --stand_alone=true

all: wumpus hello

wumpus: wumpus.pl tests.pl 
	$(CC) $(PFLAGS) -o wumpus -c tests.pl wumpus.pl

hello: hello.pl 
	$(CC) $(PFLAGS) -o hello -c tests.hello hello.pl 

tests: wumpus hello
	./wumpus > student.wumpus && diff student.wumpus tests.sol
	./hello > student.hello && diff student.hello hello.sol

clean:
	rm wumpus hello tests.out student.hello student.wumpus
