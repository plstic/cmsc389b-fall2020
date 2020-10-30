import os
test_files = range(1,6)

def preprocess():
	for i in test_files:
		file_name = str(i) + ".student"
		student_output = open(file_name)
		lines = student_output.readlines()
		c_file = "./" + str(i) + ".c"
		c_output = open(c_file, "w")
		for line in lines[1:-1]:
			c_output.write(line)		

def compile_tests():
	for i in test_files:
		command = 'gcc ' + str(i) + '.c -o ' + str(i) + '.x'
		os.system(command)
	os.system("rm *.student")

def run_tests():
	for i in test_files:
		command = 'racket -t main.rkt -m tests/' + str(i) + '.rkt > ' + str(i) + '.student'
		os.system(command)

def test_output():
	for i in test_files:
		command = './' + str(i) + '.x'
		os.system(command)
	
run_tests()
preprocess()
compile_tests()
test_output()
