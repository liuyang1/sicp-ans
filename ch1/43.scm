#lang racket
(define (compose f g) (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1)
    f
    (compose f (repeat f (- n 1)))))

(define (square x) (* x x))

; square twice on 5, (5 ^ 2) ^ 2 -> 625
((repeat square 2) 5)
