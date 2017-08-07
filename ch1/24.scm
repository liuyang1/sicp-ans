#lang racket

(require "prime-check.rkt")

; random function generate pseudo-random number [0, n)
; it only could generate in [0, 4294967087)
;
; don't save fermat test random value,
; when n is large, collision probablity is so low, we don't need to save it.
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(set-prime?! (lambda (n) (fast-prime? n 100)))

(test-case)
