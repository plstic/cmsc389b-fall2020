import os

def preprocess():
	q4 = open("q4.rkt")
	lines = q4.readlines()
	c_file = "./q4.c"
	c_output = open(c_file, "w")
	for line in lines[1:-1]:
		c_output.write(line)		

def compile_gcc():
	command = 'gcc q4.c -o q4.x'
	os.system(command)
	
def compile_racket():
	command = 'racket -t main.rkt -m test.rkt > q4.rkt'
	os.system(command)

def clean():
	command = 'rm q4.rkt q4.c'
	os.system(command)

compile_racket()
preprocess()
compile_gcc()
clean()
