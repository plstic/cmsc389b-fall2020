import os

def checkoutput(inputfile, answers):
  cmd = 'make bbcalc' 
  cmd2 = '( ./bbcalc < '+inputfile+' ) > output'

  os.system(cmd)
  os.system(cmd2)

  outputfile = open('output', 'r')
  sol = open(answers, 'r')
  for line in outputfile:
    s = sol.readline()
    if s.strip() != line.strip():
      print("got "+line[:-1]+"\texpected: "+s)

answers = "sols"
inputfile = "input"
removefiles = 'rm *.lex* *.tab.*; rm output'

checkoutput(inputfile,answers)
os.system(removefiles)
