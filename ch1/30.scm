#lang racket
(define (sum term a next b)
  (define (iter a [acc 0])
    (if (> a b)
      acc
      (iter (next a) (+ acc (term a)))))
  (iter a))

(define (id a) a)
(define (inc a) (+ a 1))
(sum id 1 inc 10)
(sum (lambda (x) (* x x))
     1 inc 10)
