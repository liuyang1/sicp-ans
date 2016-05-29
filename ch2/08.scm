#lang racket

(require "interval.scm")

(define *x* (make-interval 1 2))
(define *y* (make-interval 3 4))
(add-interval *x* *y*)
(sub-interval *x* *y*)
(sub-interval *y* *x*)
