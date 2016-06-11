#lang racket

(require "deriv.scm")

(unit-test '(- x y) 1)
(unit-test '(- y x) -1)
(unit-test '(- (* 2 x) x y) 1)
(unit-test '(- (* y x) x y) '(- y 1))

(unit-test '(sin x) '(cos x))
(unit-test '(cos x) '(- (sin x)))
(unit-test '(* (sin x) (cos x)) '(- (^ (cos x) 2) (^ (sin x) 2)))
(unit-test '(^ (cos x) 2) '(* -2 (sin x) (cos x)))
