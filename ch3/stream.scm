#lang racket
(define (prime? x)
  (define (try n)
    (cond ((> (* n n) x) #t) 
          ((= 0 (remainder x n)) #f)
          (else (try (+ n 1)))))
  (try 2))

(define empty-stream '())
(define stream-null? null?)

; DELAY 
; just pub b as a FORM, not to calc b, only return (delay b-form)
; !!!
; this should define as MACRO, not a proc
; so not to APPLY it.
(define-syntax stream-cons
  (syntax-rules () ((stream-cons a b) 
                    (cons a (delay b)))))
(define (stream-car stream) (car stream))
; FORCE
; get a form which car is DELAY, then apply the form
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enum low high)
  (if (> low high) empty-stream
    (stream-cons low (stream-enum (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

(display (stream-car (stream-cdr (stream-filter prime?
                                       (stream-enum 10000 201000)))))
(newline)
