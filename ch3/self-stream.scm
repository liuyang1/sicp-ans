; ref:http://stackoverflow.com/questions/15464485/what-am-i-doing-wrong-in-this-scheme-stream-implementation

; define my-delay and my-force by SYNTAX
(define-syntax my-delay
  (syntax-rules () ((my-delay expr) (lambda () expr))))
(define (my-force delayed-obj) (delayed-obj))

; !!!
; must define stream-cons as SYNTAX
(define-syntax stream-cons
  (syntax-rules () ((stream-cons x y)
                    (cons x (my-delay y)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (my-force (cdr stream)))

; test code
(define (range low)
  (let ((next (+ 1 low)))
  (stream-cons low (range next))))

(define *seq* (range 0))

(define (loop low high)
  (if (> low high) '()
    (begin (display (stream-car *seq*))
           (newline)
           (set! *seq* (stream-cdr *seq*))
           (loop (+ low 1) high))))
;(loop 0 10000000)

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))
(display (stream-ref *seq* 10000))
(newline)
