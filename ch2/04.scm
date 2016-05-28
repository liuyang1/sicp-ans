#lang racket
; MYCONS return one proc, which's augment is one proc.
;       This proc have two arguemnt.
(define (mycons x y) (lambda (m) (m x y)))
(define (mycar z) (z (lambda (p q) p)))
(define (mycdr z) (z (lambda (p q) q)))

(define t (mycons 1 2))
(mycar t)
(mycdr t)
