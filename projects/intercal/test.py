import os

def checkoutput(inputfile, answers):
  cmd = 'make revlist' 
  cmd2 = '( ./rev_list < '+inputfile+' ) > output'

  os.system(cmd)
  os.system(cmd2)

  outputfile = open('output', 'r')
  sol = open(answers, 'r')
  print("errors:")
  for line in outputfile:
    s = sol.readline()
    if s.strip() != line.strip():
      print("got "+line[:-1]+"\texpected: "+s)

answers = "sols"
inputfile = "input"

checkoutput(inputfile,answers)
os.system("rm output rev_list")
