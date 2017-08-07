#lang racket
(define (square a) (* a a))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)    n)
        ((divides? test-divisor n)      test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (seq fn x times)
  (if (= times 1) (list x)
    (cons x (seq fn (fn x) (- times 1)))))

(define (fn x) (+ 9 (* 10 x)))
(define *seq* (seq fn 1 10))
*seq*

(map smallest-divisor *seq*)

(define (iter f x)
  (cons x (iter f (f x))))
(define (take n iterlst)
  (if (= n 0) '()
    (cons (car iterlst)
          (take (- n 1) (cdr iterlst)))))

; (define *lazyseq* (take 10 (iter fn 1)))
; implement lazy-seq need stream related funciton, such as DELAY and FORCE.
