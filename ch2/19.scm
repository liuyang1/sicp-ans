#lang racket
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define cn-coins uk-coins)

(define (cc amount coin-values)
  (define (no-more? coin-values) (null? coin-values))
  (define (except-first-denomination coin-values) (cdr coin-values))
  (define (first-denomination coin-values) (car coin-values))
  (cond ((zero? amount) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define *comb* (cc 100 us-coins))

(define (rand-test coins v)
  (let* ((s (shuffle coins))
        (rt (cc 100 s)))
   (displayln s)
   (displayln rt)
   (= rt v)))

(map (lambda (x) (rand-test us-coins *comb*)) (range 10))
; change sequence of coins, not affect solution from CC
