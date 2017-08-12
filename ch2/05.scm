#lang racket

(define (pow a b)
  (define (square x) (* x x))
  (cond ((= b 0) 1)
        ((= b 1) a)
        ((even? b) (square (pow a (/ b 2))))
        (else (* a (pow a (- b 1))))))

; find X have factor F times
(define (fact-times x f)
  (define (help x acc)
    (if (= (remainder x f) 0)
      (help (/ x f) (+ acc 1))
      acc))
  (help x 0))

(define (mycons a b) (* (pow 2 a)
                        (pow 3 b)))
(define (mycar x)
  (fact-times x 2))
(define (mycdr x)
  (fact-times x 3))

(define *v* (mycons 5 7))
*v*
(mycar *v*)
(mycdr *v*)
