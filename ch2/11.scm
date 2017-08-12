#lang racket
(require "interval.scm")

(define *pos* (make-interval 2 5))
(define *neg* (make-interval -3 -7))
(define *zer* (make-interval -1 1))
(define *lst* (list *pos* *neg* *zer*))

(define (test-case x y)
  (letrec ((v0 (mul-interval-trival x y))
           (v1 (mul-interval x y))
           (rt (eq-interval? v0 v1)))
    (display (format "~a * ~a = ~a ?= ~a ~a~n" x y v0 v1 rt))
    rt))

(define (combine ls0 ls1)
  (define (hl x ls)
    (if (null? ls)
      '()
      (cons (cons x (car ls))
            (hl x (cdr ls)))))
  (if (null? ls0)
    '()
    (append (hl (car ls0) ls1)
            (combine (cdr ls0) ls1))))

(define (combine1 xs ys)
  (foldr append '()
         (map (lambda (x)
                (map (lambda (y) (cons x y)) ys))
              xs)))

; AND is a macro instead of a procdurce. so cannot write like this style
; (apply and '(#t #t #f))
; This is for short-eval, to avoid eval all arguments
; use ANDMAP instead
(define (test-all ls)
  (andmap identity (map (lambda (x) (test-case (car x) (cdr x)))
                        ls)))

(test-all (combine *lst* *lst*))

(define *ls1* (list (make-interval -2 3)
                    (make-interval -1 1)
                    (make-interval -4 1)
                    (make-interval -1 4)))

(test-all (combine *ls1* *ls1*))
