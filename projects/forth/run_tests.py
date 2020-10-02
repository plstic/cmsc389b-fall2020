import os

def checkoutput(answers):
  cmd = 'gforth cards.fs tests.fs -e bye' 

  os.system(cmd +" > output")

  outputfile = open('output', 'r')
  sol = open(answers, 'r')
  print("errors: ")
  for line in outputfile:
    s = sol.readline()
    if s.strip() != line.strip():
      print("got "+line[:-1]+"\texpected: "+s)

answers = "sols"
removefiles = 'rm output'

checkoutput(answers)
os.system(removefiles)
