#lang racket
(define (filt-acc comb unit term a next b filt)
  (define (acc v)
    (if (>= v b) unit
      (if (filt v)
        (comb (term v) (acc (next v)))
        (acc (next v)))))
  (acc a))

(define (inc a) (+ a 1))
(define (id a) a)
(define (TRUE a) #t)

(define (find-divisor n test-divisor)
  (define (square a) (* a a))
  (define (next-divisor val) (if (<= val 2) (+ val 1) (+ val 2)))
  (define (divides? a b) (= (remainder b a) 0))
  (cond ((> (square test-divisor) n)    n)
        ((divides? test-divisor n)      test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (prime? n)
  (= n (find-divisor n 2)))

(filt-acc cons '() id 1 inc 10 TRUE)
(filt-acc + 0 id 1 inc 10 TRUE)

(filt-acc cons '() id 2 inc 10 prime?)
(filt-acc + 0 id 2 inc 10 prime?)

(define (prod-prime n)
  (define (gcd? a) (= (gcd a n) 1))
  (filt-acc * 1 id 2 inc n gcd?))
(prod-prime 7)
(prod-prime 6)
