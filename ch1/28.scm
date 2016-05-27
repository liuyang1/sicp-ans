#lang racket

(require "prime-check.rkt")

(define (isord2? a n)
  (and (not (= a 1)) (not (= (- a 1) n)) (= (remainder (square a) n) 1)))

(define (miller-rabin n)
  (define (try-it a)
    (and (not (isord2? a n)) (= (expmod a (- n 1) n) 1)))
  (try-it (+ 2 (random (- n 2)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin n) (fast-prime? n (- times 1)))
        (else false)))

(define (miller-rabin-prime? n) (fast-prime? n 100))

(define (is-miller-rabin-wrong? x)
  (xor (miller-rabin-prime? x) (prime? x)))

(define *seq* (range 3 200))
(filter prime? *seq*)
(filter is-miller-rabin-wrong? *seq*)
; NOTICE: miller-rabin is easy to refuse little prime

; check Carmichael number
(not (prime? 561))

(set-prime?! (lambda (n) (fast-prime? n 100)))

(test-case)
