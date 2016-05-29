#lang racket

(require "interval.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
   (div-interval one
                 (add-interval (div-interval one r1)
                               (div-interval one r2)))))

(define *r1* (make-interval 2 3))
(define *r2* (make-interval 5 7))

(par1 *r1* *r2*)
(par2 *r1* *r2*)

(div-interval *r1* *r1*)
(div-interval *r1* *r2*)

; R1=A(1+-a), R2=B(1+-b)
; R1/R1 = A(1+-a)/A(1+-a)
;       = [(1-a)/(1+a), (1+a)/(1-a)]
;       = 1+-(2a)/(1-a^2)
; same proc
; R1/R2 = A/B[1+-(a+b)/(1+ab)]
