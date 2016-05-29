#lang racket

(require "interval.scm")

(define *x* (make-interval 1 2))
(define *y* (make-interval 3 4))

(= (width-interval (add-interval *x* *y*))
   (+ (width-interval *x*)
      (width-interval *y*)))
(= (width-interval (sub-interval *x* *y*))
   (+ (width-interval *x*)
      (width-interval *y*)))

(mul-interval *x* *y*)
(width-interval (mul-interval *x* *y*))
(div-interval *y* *x*)
(width-interval (div-interval *y* *x*))
