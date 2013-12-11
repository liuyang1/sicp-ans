; basic proc
(define empty-stream '())
(define stream-null? null?)

(define-syntax stream-cons
  (syntax-rules () ((stream-cons x y)
                    (cons x (delay y)))))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define (range low)
  (let ((next (+ 1 low)))
  (stream-cons low (range next))))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else
          (stream-filter pred (stream-cdr stream)))))

; sieve algo to generate prime inf-stream
(define (sieve stream)
  (stream-cons (stream-car stream)
               (sieve 
                 (stream-filter 
                   (lambda (x) (not (= 0 (remainder x (stream-car stream)))))
                   (stream-cdr stream)))))

; test code
(define *primes* (sieve (range 2)))
(display (stream-ref *primes* 50))
(newline)
