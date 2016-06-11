#lang racket

(require "deriv.scm")

(unit-test '(+ x y) 1)
(unit-test '(* (* x y) (+ x 3)) '(* (+ (* 2 x) 3) y))
(unit-test '(* x x) '(* 2 x))
(unit-test '(+ x (+ y (+ x 3))) 2)
(unit-test '(* x y) 'y)

(unit-test '(^ x 2) '(* 2 x))
