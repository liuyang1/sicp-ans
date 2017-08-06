#lang racket
(define (Ackermann x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else    (Ackermann (- x 1)
                            (Ackermann x (- y 1))))))

(Ackermann 1 10)
(Ackermann 2 4)
(Ackermann 3 3)

(define (range start stop step)
  (if (> start stop) '()
    (cons start (range (+ start step) stop step))))

(define (f n) (Ackermann 0 n))
(define (g n) (Ackermann 1 n))
(define (h n) (Ackermann 2 n))
(define (k n) (* 5 n n))

(define *seq* (range 0 4 1))
*seq*
; 2n
(map f *seq*)
; 2^n
(map g *seq*)
; 2^(2^(...^2))
; ......... count n
(map h *seq*)
; 5n^2
(map k *seq*)
