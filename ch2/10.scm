#lang racket

(require "interval.scm")

(define *x* (make-interval -1 2))
(define *y* (make-interval 3 4))
(div-interval-zero *y* *x*)
(div-interval *y* *x*)
