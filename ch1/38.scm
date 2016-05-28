#lang racket
(require "cont-frac.scm")

(define (e-ni i) 1.0)
(define (e-ki i)
  (if (= 0 (remainder (+ i 1) 3))
    (* 2 (/ (+ i 1) 3))
    1))

(+ 2 (cont-frac e-ni e-ki 10000))
