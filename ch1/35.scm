#lang racket

(require "fixed.scm")

(fixed cos 1.0)
(fixed (lambda (x) (+ 1 (/ 1 x))) 1.0)
