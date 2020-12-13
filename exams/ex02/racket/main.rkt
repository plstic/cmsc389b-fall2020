#lang racket
(provide (all-defined-out))
(require (only-in "qfour.rkt" compile))

(define (main fn)
  (with-input-from-file fn
    (λ ()
      (let ((p (read-program)))
        (display (compile p))))))

(define (read-program)
  (regexp-match "^#lang racket" (current-input-port))
  (read))

