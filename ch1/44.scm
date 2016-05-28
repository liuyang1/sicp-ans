#lang racket
(define (smooth f)
  (define dx 0.1)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (compose f g) (lambda (x) (f (g x))))

(define (repeat f n)
  (if (= n 1) f (compose (repeat f (- n 1)) f)))

(define (smooth-n n f) ((repeat smooth n) f))

(define (square x) (* x x))

(square 5)
((smooth square) 5)
((smooth (smooth square)) 5)
((smooth-n 2 square) 5)
((smooth-n 3 square) 5)
