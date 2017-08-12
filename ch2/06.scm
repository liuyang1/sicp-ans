#lang racket
;;; Church Number
; Y F = F (Y F), It looks like Y-combinator. ???
;;;
;;; After learned SICP, LISP, Haskell, Abstract Algebra, this is interestring.
;;;
;;; (x, f) is a gourp, x is UNIT of group, f is action of group.
;;;
;;; Haskell have currying function and type-system, so easier than LISP.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (plus m n) (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define (mul m n) (lambda (f) (lambda (x) ((m (n f)) x))))

zero
(add-1 zero)
one
(plus one two)

; (define pure-zero 0)
; (define (pure-inc x) (+ 1 x))

(define pure-zero '(0))
(define (pure-inc x) (cons (+ 1 (car x)) x))

((zero pure-inc) pure-zero)
(((add-1 zero) pure-inc) pure-zero)
((one pure-inc) pure-zero)
((two pure-inc) pure-zero)
(((plus two one) pure-inc) pure-zero)
(((plus two three) pure-inc) pure-zero)
(((mul two three) pure-inc) pure-zero)
(((mul three three) pure-inc) pure-zero)
