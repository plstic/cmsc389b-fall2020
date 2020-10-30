all: bbcalc

%.c: %.y
%.c: %.l

bbcalc: big-brane-calc.l big-brane-calc.y big-brane-calc.h big-brane-calc.c
	bison -d big-brane-calc.y
	flex -obig-brane-calc.lex.c big-brane-calc.l
	gcc -o $@ big-brane-calc.tab.c big-brane-calc.lex.c big-brane-calc.c

test:
	python test.py
clean: 
	rm *.lex.c lex.yy.c *.tab.* bbcalc
