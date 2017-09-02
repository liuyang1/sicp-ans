#lang racket

(provide (all-defined-out))

; flatmap :: (a -> [b]) -> [a] -> [b]
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high) '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate-iter op init seq)
  (define (helper seq ret)
    (if (null? seq) ret
      (helper (cdr seq) (op (car seq) ret))))
  (helper seq init))

(define (accumulate-rec op initial sequence)
  (if (null? sequence) initial
    (op (car sequence)
        (accumulate-rec op initial (cdr sequence)))))

(define accumulate accumulate-iter)
