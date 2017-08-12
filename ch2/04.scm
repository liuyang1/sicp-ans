#lang racket
; MYCONS return one proc, which's augment is one proc.
;       This proc have two arguemnts.
; mycons :: a -> b -> ((a -> b -> c) -> c)
(define (mycons x y) (lambda (m) (m x y)))
; mycar :: ((b -> c -> b) -> a) -> a
(define (mycar z) (z (lambda (p q) p)))
; mycdr :: ((b -> c -> c) -> a) -> a
(define (mycdr z) (z (lambda (p q) q)))

(define t (mycons 1 2))
(mycar t)
(mycdr t)
