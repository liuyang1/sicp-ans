#lang racket
(require "02.scm")

(define make-rect-diag cons)
(define left-point-diag car)
(define right-point-diag cdr)
(define (vector-diag rect)
  (sub-point (left-point-diag rect)
             (right-point-diag rect)))

(define (test-case-rect make-rect vector-rect)
  (define (circum rect)
    (let ((v (vector-rect rect)))
     (* 2 (+ (abs (x-point v))
             (abs (y-point v))))))
  (define (area rect)
    (let ((v (vector-rect rect)))
     (abs (* (x-point v)
             (y-point v)))))

  (define *rect* (make-rect (make-point 1 2)
                            (make-point 3 -2)))
  (print-point (vector-rect *rect*))
  (displayln (circum *rect*))
  (displayln (area *rect*)))

(test-case-rect make-rect-diag vector-diag)

(define (make-rect-rlt A B)
  (cons A
        (sub-point B A)))
(define (vector-rlt rect) (cdr rect))

(test-case-rect make-rect-rlt vector-rlt)
