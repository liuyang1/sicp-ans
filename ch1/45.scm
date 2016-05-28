#lang racket

(require "fixed.scm")

(define (damp func)
  (lambda (x)
    (/ (+ x (func x)) 2)))

(define (func x) (/ 2.0 x))

; try to find sqrt(2.0) with damp-fixed solution
; assume init with X, it always jump between X and 2.0/X as dead loop
; (fixed-show func 1)
; solve it by damp-fixed soution
(fixed-show (damp func) 2)

; also try to find sqrt3(2.0)
(fixed-show (damp (lambda (x) (/ 2.0 x x))) 2)

; try to find pow(2.0, 1/4)
; dead-loop again
(define (fn x) (/ 2.0 x x x))
; (fixed-show (damp fn) 2)
; good when use twice damp
(fixed-show (damp (damp fn)) 2)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeat f n)
  (if (<= n 1)
    f
    (compose (repeat f (- n 1)) f)))

; generate y |-> x / y ^ (n - 1) func
(define (gen-root-n x n)
  (lambda (y) (/ x
                 ((repeat (lambda (a) (* a y))
                          (- n 1))
                  1))))

(fixed-show ((repeat damp 6) (gen-root-n 2.0 64))
            2)
; summary
; | order | damp |
; | 2-3   | 1    |
; | 4-7   | 2    |
; | 8-15  | 3    |
; | 16-31 | 4    |
; | 32-63 | 5    |

; so it need [log2(N)] times damp
