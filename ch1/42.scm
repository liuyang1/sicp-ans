#lang racket
(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

; (x + 1) ^ 2 => 49
((compose square inc) 6)
; x ^ 2 + 1 => 37
((compose inc square) 6)
