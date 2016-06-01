#lang racket
(define (same-parity first . other)
  (let ((proper (even? first)))
   (define (ip other res)
     (if (null? other) res
     (let ((one (car other)))
      (if (equal? proper (even? one)) 
        (ip (cdr other) (append res (list one)))
        (ip (cdr other) res)))))
   (cons first (ip other '()))))

(define (same-parity-2 first . other)
  (let ((proper (even? first)))
  (define (same first other)
    (if (null? other) '()
      (if (equal? proper (even? (car other)))
        (cons (car other) (same first (cdr other)))
        (same first (cdr other)))))
  (cons first (same first other))))

(define (test-case fn)
  (displayln fn)
  (displayln (fn 1 2 3 4 5 6 7))
  (displayln (fn 2 3 4 5 6 7)))

(test-case same-parity)
(test-case same-parity-2)
