all: flex wc mf-wc states concordance cref smcalc bbcalc

flex: flex-calc.l
	flex flex-calc.l
	gcc -o $@ lex.yy.c -lfl

wc: wc.l
	flex wc.l
	gcc -o $@ lex.yy.c -lfl

mf-wc: multi-file-wc.l
	flex multi-file-wc.l
	gcc -o $@ lex.yy.c -lfl

states: start-states.l
	flex start-states.l
	gcc -o $@ lex.yy.c -lfl

concordance: symbol-table.l
	flex symbol-table.l
	gcc -o $@ lex.yy.c -lfl

cref: c-ref.l
	flex c-ref.l
	gcc -o $@ lex.yy.c -lfl

calc: simp-calc.l simp-calc.y
	bison -d simp-calc.y
	flex simp-calc.l
	gcc -o $@ simp-calc.tab.c lex.yy.c -lfl

clean: 
	rm *.lex.c lex.yy.c *.tab.* wc flex mf-wc states concordance cref 
