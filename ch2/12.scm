#lang racket

(require "interval.scm")

(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))

(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (width x)
  (- (upper-bound x) (lower-bound x)))
(define (precent x)
  (/ (width x) (center x) 2))

(define *x* (make-center-percent 4 0.05))
*x*
(precent *x*)
