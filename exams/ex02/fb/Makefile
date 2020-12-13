all: q4

q4: qfour.l qfour.y
	bison -d qfour.y
	flex qfour.l
	gcc -o $@ qfour.tab.c lex.yy.c -lfl

clean: 
	rm *.lex.c lex.yy.c *.tab.* wc q4
