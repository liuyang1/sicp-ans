#lang racket

(provide (all-defined-out))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a ) 0))

(define (set-next-fn! fn) (set! next-fn fn))
; place-holder may change by other module
(define (next-fn x) (+ 1 x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-fn test-divisor)))))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (current-inexact-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n start-time (current-inexact-milliseconds))
    #f))

(define (report-prime n start-time end-time)
  (display n)
  (display " *** ")
  (display (- end-time start-time))
  (newline))

(define (search-for-primes begin-value num)
  (define (hl bg num)
    (cond ((= num 0) #t)
          ((not (timed-prime-test bg)) (hl (+ 2 bg) num))
          (else (hl(+ 2 bg) (- num 1)))))
  (cond ((even? begin-value) (hl (+ 1 begin-value) num))
        (else (hl begin-value num))))

(define (test-case)
  (search-for-primes 1000 3)
  (search-for-primes (* 1000 10) 3)
  (search-for-primes (* 1000 100) 3)
  (search-for-primes (* 1000 1000) 3))
