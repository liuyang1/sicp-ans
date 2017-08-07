#lang racket
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(displayln (new-if (= 2 3) 0 5))
(displayln (new-if (= 1 1) 0 5))

; this new-if, it is not special format, so it have to evaluate out <the-clause>
; and <else-caaluse>.
; When use it at <sqrt-iter> function, it always evaluate out <sqrt-iter> clause.
; so never stop and have NO chance to return.

; verify with 05.scm
(define (p) (p))

(define (test-if x p)
  (if (= x 0) 0 (p)))

; 1st, could output normally
(displayln (test-if 0 p))
; 2nd, dead-loop
; (displayln (test 1 p))

(define (test-new-if x p)
  (new-if (= x 0) 0 (p)))

; 1st, also dead-loop
(displayln (test-new-if 0 p))
