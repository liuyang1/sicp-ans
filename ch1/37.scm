#lang racket
(require "cont-frac.scm")

(define one (lambda (x) 1.0))

(cont-frac one one 10)
(cont-frac-rec one one 10)

(define (cont-frac-step k)
  (cont-frac one one k))

(define *phi* (/ 2 (+ 1 (sqrt 5))))
*phi*

(define (close? a b)
  (< (abs (- a b)) 0.0001))

(define (find-step)
  (define (try k)
    (if (close? (cont-frac-step k) *phi*)
      k
      (try (+ k 1))))
  (try 1))

(find-step)
