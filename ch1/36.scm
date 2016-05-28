#lang racket
(define (fixed f first)
  (define tolerance 0.00001)
  (define (close? v1 v2) (< (abs (/ (- v1 v2) (+ v1 v2) 2))
                            tolerance))
  (define (try guess)
    (begin (displayln guess)
           (let ((next (f guess)))
            (if (close? guess next)
              next
              (try next)))))
  (try first))

(define (func x) (/ (log 1000) (log x)))
; high order func
(define (damp func)
  (define (proc x) (/ (+ x  (func x)) 2))
  proc)

(displayln "test func")
(fixed func 5)
(displayln "test damp func")
(fixed (damp func) 5)
