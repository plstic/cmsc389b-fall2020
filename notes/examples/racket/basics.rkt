#lang racket
; primitive values
2
1
1.0
8.0
"Cliff"
"Justin"
#\A
#\space
#\u03BB
#t
#f

; a symbol is can be made with anything. You just need to quote it.
; Basically says the the thing is data and not code
'"Fred"
'1
'x
'true ; Racket prefers you to use #t for true so this will not be equal to #t
'#t
'+
'()

; let exptressions
(let ((x 4)) x)
(let ((x 3) (y 4)) (+ x y))

;functions
(+ 2 3) ; prefix notation
(define (name var1 var2) (+ var1 var2))
(name 2 3)

; anonymouse functions
((lambda (x y) (+ x y)) 2 3)
