#lang racket
(require "fixed.scm")

(define (func x) (/ (log 1000) (log x)))
; high order func
(define (damp func)
  (define (proc x) (/ (+ x  (func x)) 2))
  proc)

(displayln "test func")
(fixed-show func 5)
(displayln "test damp func")
(fixed-show (damp func) 5)
