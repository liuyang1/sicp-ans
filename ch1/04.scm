#lang racket
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; function as first class.
; so return + or - as operation func.

(display (a-plus-abs-b 3 -2))
(newline)
