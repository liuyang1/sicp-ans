#lang racket

(require "deriv.scm")

(unit-test '(+ x x x) 3)
(unit-test '(* x y (+ x 3)) '(* (+ (* 2 x) 3) y))
