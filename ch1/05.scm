#lang racket
(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

; if statement: first apply Predicate, then apply sub-statement as result of
; Predicate

; applicative sequence
; interp first try to apply on operator and every openrands, then apply
; openrator on openrands.

; regular sequence
; Do not apply value of every object, but try to replace or expand as algebra,
; until finally need their value.


; for this statement,
; applicative sequence, interp will loop apply on (p) try to get result of p.
;       so there is a dead loop.
; regular sequence, interp will not apply on (p), will directly get result 0.
(test 0 (p))
