CC = cobc
TESTS = pub-hello pub01 pub02 pub03

all: hello besbubo

test:
	make clean
	make all
	$(foreach var,$(TESTS),./run_test.sh $(var) >> output.txt;)

hello: hello.cob
	$(CC) -x hello.cob

besbubo: besbubo.cob
	$(CC) -x besbubo.cob

clean:
	rm -f hello besbubo output.txt
