all: revlist 

revlist: rev_list.i
	ick rev_list.i
test: 
	ick rev_list.i && (./rev_list < input)>out.student && diff out.student sols 
clean:
	rm rev_list out.student rev_list
