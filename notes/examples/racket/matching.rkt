(define (x) ('(1 2 3 4 5)))
(let ([x '(1 2 3 4 5)]) 
	(match x
		['() 0]
		[(cons x y) (* x x)])) ; returns 1

(let ([x '(1 2 3 4 5)]) 
	(match x
		['(1 2 4) #f]
		['(1 2 3 4 5) #t])) ; returns #t

; quasiquoting and unquoting
(define (quote_ex x)
	(match x
		[`(,a ,b) (- (* a a) (* b b))]
		[`(1 2 ,a 4 5) a]
		))
(quote_ex '(1 2 3 4 5)) ; returns 3
(quote_ex '(1 2 6 4 5)) ; returns 6
(quote_ex '(1 2 )) ; returns -3

; we can also ask for types

(define (is_char? x)
	(match x
		[(? char? c) #t]
		[_ #f]))
