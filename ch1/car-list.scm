#lang racket

; list mod sqrt for Carmichael number
(define *n* 651)
(define *seq* (range 2 (- *n* 1)))

(define (is-mod-sqrt? x n)
  (and (not (= x 1))
       (not (= x (- n 1)))
       (= (remainder (* x x) n) 1)))

(filter (lambda (x) (is-mod-sqrt? x *n*))
        *seq*)
