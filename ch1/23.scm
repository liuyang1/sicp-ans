#lang racket

(require (file "prime-check.rkt"))

(define (next val)
  (if (<= val 2)
    (+ val 1)
    (+ val 2)))
(set-next-fn! next)

(test-case)
