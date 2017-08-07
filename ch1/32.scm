#lang racket
(define (accumulate-rec combiner unit-value term a next b)
  (if (> a b)
    unit-value
    (combiner (term a)
              (accumulate-rec combiner unit-value term (next a) next b))))

(define (accumulate-iter combiner unit-value term a next b)
  (define (ip a result)
    (if (> a b)
      result
      (ip (next a) (combiner (term a) result))))
  (ip a unit-value))

; test code
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (prod term a next b)
  (accumulate * 1 term a next b))

(define (range term a next b)
  (accumulate cons '() term a next b))

(define (id a) a)
(define (inc a) (+ a 1))

(define (fact n) (prod id 1 inc n))
(define (accu n) (sum id 1 inc n))
(define (rang n) (range id 1 inc n))

(define (test-case)
  (let ((n 10))
   (begin
     (displayln (rang n))
     (displayln (accu n))
     (displayln (fact n))
     #t)))

(define accumulate accumulate-iter)
(test-case)

(set! accumulate accumulate-rec)
(test-case)
