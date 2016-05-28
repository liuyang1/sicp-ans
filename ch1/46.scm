#lang racket

(define (iter-impr close? improve)
  (define (ip x)
    (let ((next (improve x)))
     (if (close? next x)
       next
       (ip next))))
  ip)

(define (close? v1 v2) (< (abs (- v1 v2)) 0.001))

(define (mysqrt x)
  (define (improve guess) (/ (+ guess (/ x guess)) 2.0))
  ((iter-impr close? improve) x))

(define (fixed f first)
  (define (improve guess) (f guess))
  ((iter-impr close? improve) first))

(mysqrt 2)
(fixed cos 1.0)
