#lang racket
(define (sum term a next b)
  (define (iter a [acc 0])
    (if (> a b)
      acc
      (iter (next a) (+ acc (term a)))))
  (iter a))

(define (func a) a)
(define (inc a) (+ a 1))
(sum func 1 inc 10)
