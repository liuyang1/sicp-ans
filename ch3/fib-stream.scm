#lang racket
(define empty-stream '())
(define stream-null? null?)
(define-syntax stream-cons
  (syntax-rules () ((stream-cons a b) 
                    (cons a (delay b)))))
(define stream-car car)
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc s1 s2)
  (if (or (stream-null? s1) (stream-null? s2)) empty-stream
    (stream-cons (proc (stream-car s1) (stream-car s2))
                 (stream-map proc (stream-cdr s1) (stream-cdr s2)))))
(define (add-stream s1 s2)
  (stream-map + s1 s2))

; fib(n) = fib(n-1) + fib(n-2)
; but this complexity is O(1)
; because this hold status at *fib*, and only create one sequence.
; and not need to re-calc it when ref to prior data
(define *fibs*
  (stream-cons 0
               (stream-cons 1
                            (add-stream (stream-cdr *fibs*)
                                        *fibs*))))

; test code
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (loop start end)
  (if (> start end) '()
    (begin (display (stream-ref *fibs* start))
           (newline)
           (loop (+ start 1) end))))

(stream-ref *fibs* 10)
(display *fibs*)
