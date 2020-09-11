CC = cobc

all: hello besbubo

test:
	make all
	rm -f output.txt
	./run_tests.sh

hello: hello.cob
	$(CC) -x hello.cob

besbubo: besbubo.cob
	$(CC) -x besbubo.cob

clean:
	rm hello besbubo
