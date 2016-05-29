#lang racket
(require "interval.scm")

(define *pos* (make-interval 2 5))
(define *neg* (make-interval -3 -7))
(define *zer* (make-interval -1 1))
(define *lst* (list *pos* *neg* *zer*))

(define (test-case x y)
  (display x)
  (display "\t")
  (display y)
  (display "\t")
  (let ((rt (eq-interval? (mul-interval-trival x y)
                          (mul-interval x y))))
    (displayln rt)
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

; AND is a macro instead of a procdurce. so cannot write like this style
; (apply and '(#t #t #f))
; This is for short-eval, to avoid eval all arguments
; use ANDMAP instead
(define (test-all ls)
  (andmap identity (map (lambda (x) (test-case (car x) (cdr x))) ls)))

(test-all (combine *lst* *lst*))

(define *ls1* (list (make-interval -2 3)
                    (make-interval -1 1)
                    (make-interval -4 1)
                    (make-interval -1 4)))

(test-all (combine *ls1* *ls1*))
