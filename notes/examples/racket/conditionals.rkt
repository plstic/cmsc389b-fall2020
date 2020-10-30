; (if bool_expr true_branch false_branch)
(if #t 1 0)
(cond
	[(= 2 3) 'wrong]
	[(= 2 5) 'incorrect]
	[(= 2 2) 'ok]
	[else 'unreachable])

(define (rec_search lst)
	(cond 
		[(null? lst) #f]
		[(eq? 5 (car lst))]							; car gets head of list
		[else (rec_search (cdr lst))])) ; cdr gets tail of list

