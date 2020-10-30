#lang racket
(provide (all-defined-out))

(define stdio `("\n#include <stdio.h>\n"))
(define stdlib `("\n#include <stdlib.h>\n"))
(define main-header`("int main (){\n\tint a = "))
(define printf `(,(string-append ";\n\tprintf(" (string #\" #\% #\d #\\ #\n #\" #\,) "a);\n}\n")))

(define (compile e)
	(append stdio stdlib main-header (compile-e e) printf))

(define (compile-e e)
	(match e
      [(? integer? i) `(,i)]
				 ))
