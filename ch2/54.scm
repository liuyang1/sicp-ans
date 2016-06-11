#lang racket

(define (my-equal? a b)
  (cond ((and (list? a) (list? b))
         (cond ((and (null? a) (null? b)) #t)
               ((and (not (null? a)) (not (null? b))
                     (and (my-equal? (car a) (car b))
                          (my-equal? (cdr a) (cdr b)))))
               (else #f)))
        ((and (number? a) (number? b)) (= a b))
        ((and (symbol? a) (symbol? b)) (eq? a b))
        (else #f)))

(define (test-case fn)
  (and (eq? #f (fn '(this is a list) '(this (is a) list)))
       (eq? #t (fn '(this is a list) '(this is a list)))
       (eq? #t (fn '() '()))
       (eq? #f (fn '() '(this is a list)))
       (eq? #t (fn '(this (is an (embeded) list)) '(this (is an (embeded) list))))
       (eq? #f (fn '(this (is an (one) list)) '(this (is an (1) list))))))

(test-case my-equal?)
