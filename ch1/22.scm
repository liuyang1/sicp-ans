#lang racket

(require (file "prime-check.rkt"))

(define (next x) (+ 1 x))
(set-next-fn! next)

(test-case)
