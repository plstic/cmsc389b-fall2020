CC = swipl
PFLAGS = --goal=main --stand_alone=true

all: hello 

hello: hello.pl
	$(CC) $(PFLAGS) -o hello -c hello.pl

clean:
	rm hello 
