#lang racket
(define (same-parity first . other)
  (let ((proper (even? first)))
   (define (ip xs res)
     (if (null? xs) res
       (let ((x (car xs)))
        (if (equal? proper (even? x))
          (ip (cdr xs) (append res (list x)))
          (ip (cdr xs) res)))))
   (cons first (ip other '()))))

(define (same-parity-2 first . other)
  (let ((proper (even? first)))
   (define (same xs)
     (cond ((null? xs) '())
           ((equal? proper (even? (car xs))) (cons (car xs)
                                                   (same (cdr xs))))
           (else (same (cdr xs)))))
   (cons first (same other))))

(define (test-case fn)
  (displayln fn)
  (displayln (fn 1 2 3 4 5 6 7))
  (displayln (fn 2 3 4 5 6 7)))

(test-case same-parity)
(test-case same-parity-2)
