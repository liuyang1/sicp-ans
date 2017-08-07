#lang racket
; define one function
; when n < 3, f(n) = n
; else, f(n) = f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3)
(define (f-rec n)
  (if (< n 3) n
    (+ (f-rec (- n 1))
       (* 2 (f-rec (- n 2)))
       (* 3 (f-rec (- n 3))))))

; iterative verison
; using f(n-1) f(n-2) f(n-3) as function arguments
(define (f-iter n)
  (define (helper num fn1 fn2 fn3)
    (let ((val (+ fn1 (* 2 fn2) (* 3 fn3))))
     (if (= num n) val
       (helper (+ num 1) val fn1 fn2))))
  (if (< n 3) n
    (helper 3 2 1 0)))

(define *seq* (range 10))
(map (lambda (f)
       (map f *seq*))
     (list f-rec f-iter))
