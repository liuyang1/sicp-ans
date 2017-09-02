#lang racket

(require "base.scm")

(define (unique-3-pairs n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-3 s n)
  (define (isSum3? lst)
    (= s (accumulate + 0 lst)))
  (filter isSum3? (unique-3-pairs (- n 3))))

(sum-3 12 10)
