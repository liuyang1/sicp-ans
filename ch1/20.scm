#lang racket
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 206 40)
; 206 40
; 40 6
; 6 4
; 4 2
; 2 0 -> 2

; THREE time remainder operation under application order.

; When use REGULAR oder, FOUR time remainder operation, but it fail when try
; (remainder 2 0) last time
