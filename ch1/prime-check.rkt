#lang racket

(provide (all-defined-out))

(define (set-next-fn! fn) (set! next-fn fn))
(define (set-prime?! fn) (set! prime? fn))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a ) 0))

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
  (printf "~A *** ~A~n" n (- end-time start-time)))

(define (search-for-primes begin-value num)
  (define (helper bg num)
    (if (= num 0) #t
      (helper (+ bg 2)
              (if (not (timed-prime-test bg))
                num
                (- num 1)))))
  (helper (if (even? begin-value) (+ begin-value 1) begin-value)
          num))

(define (seq fn x times)
  (if (= times 1) (list x)
    (cons x (seq fn (fn x) (- times 1)))))
(define (iter x) (* 10 x))
(define *seq* (seq iter 10 10))

(define (test-case)
  (map (lambda (x) (search-for-primes x 3))
       *seq*))

; for fermant and miller-rabin algo
(define (expmod base expr m)
  (cond ((= expr 0) 1)
        ((even? expr)
         (remainder (square (expmod base (/ expr 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- expr 1) m))
                     m))))
