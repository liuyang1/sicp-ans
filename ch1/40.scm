#lang racket
(require "fixed.scm")

(define (cubic a b c)
  (lambda (x) (+ (* x (+ (* x (+ x a)) b)) c)))

(define (newtons func guess)
  (define dx 0.0001)
  (define (deriv g)
    (lambda (x)
      (/ (- (g (+ x dx)) (g x)) dx)))
  (define (newton-trans g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed (newton-trans func) guess))

; should return 1
(newtons (cubic 1 1 1) 1)
