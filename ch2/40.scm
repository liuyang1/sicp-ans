#lang racket

(require "base.scm")

(define (find-divisor n test-divisor)
  (define (square a) (* a a))
  (define (next-divisor val) (if (<= val 2) (+ val 1) (+ val 2)))
  (define (divides? a b) (= (remainder b a) 0))
  (cond ((> (square test-divisor) n)    n)
        ((divides? test-divisor n)      test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (prime? n)
  (= n (find-divisor n 2)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(map displayln (prime-sum-pairs 11))
