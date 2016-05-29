#lang racket

(define make-interval cons)
(define (lower-bound x)
  (min (car x) (cdr x)))
(define (upper-bound x)
  (max (car x) (cdr x)))

(define *interval* (make-interval 4 3))
(lower-bound *interval*)
(upper-bound *interval*)
