#lang racket
(require "prime-check.rkt")

; Carmichael
(define (carmichael? n)
  (define (ismod? a n)
    (= (expmod a n n) a))
  (define (loopcheck a n)
    (cond ((= a n) #t)
          ((ismod? a n) (loopcheck (+ a 1) n))
          (else #f)))
  (and (loopcheck 2 n) (not (prime? n))))

(not (carmichael? 100))
(not (carmichael? 27))
(not (carmichael? 97))

; carmichael number is not prime number,
; but can pass fermat-test
(filter carmichael? (range 3 10000))
