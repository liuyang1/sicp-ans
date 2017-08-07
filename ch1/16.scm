#lang racket
(define (fast-expt b n)
  (define (process a b n)
    (cond ((= n 0)      a)
          ((even? n)    (process a (* b b) (/ n 2)))
          (else         (process (* a b) b (- n 1)))))
  (process 1 b n))

; for test
(fast-expt 2 3)
(fast-expt 3 4)

; keep (a * b ^ n) not change
; when n == 0, then get final result
