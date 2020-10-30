(define (product x)
	(if (<= x 0) 0 (if (= x 1) 1 (product (- x 1)))))

(define (is_even? x)
	(if (= x 0) #t (is_odd? (- x 1))))

(define (is_odd? x)
	(if (= x 0) #f (is_even? (- x 1))))
