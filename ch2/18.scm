#lang racket
(define (myreverse lst)
  (define (ip in out)
    (if (null? in)
      out
      (ip (cdr in) (cons (car in) out))))
  (ip lst '()))


(define (myreverse-rec lst)
  (if (null? lst)
    '()
    (append (myreverse-rec (cdr lst)) (list (car lst)))))

(define (test-case fn)
  (displayln (fn '()))
  (displayln (fn (list 0)))
  (displayln (fn (list 0 1)))
  (displayln (fn (list 0 1 2)))
  #t)

(test-case myreverse)
(test-case myreverse-rec)
