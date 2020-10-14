UNSAFE = -fno-pie -no-pie -fno-stack-protector -z execstack 
CC = rustc
VC = gcc

all: hello vuln1 

hello: hello.rs
	$(CC) hello.rs

vuln1: vuln.c
	$(VC) $(UNSAFE) vuln.c -o vuln.x

clean:
	rm hello *.x
