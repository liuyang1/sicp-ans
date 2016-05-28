#lang racket
(define (double func)
  (lambda (x) (func (func x))))

(define (inc x) (+ x 1))

((double inc) 5)
; 2 ^ (2 ^ 2) = 16 times inc func
(((double (double double)) inc) 5)
