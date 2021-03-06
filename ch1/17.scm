#lang racket
(define (double a) (* a 2))
(define (halve a) (/ a 2))

(define (asterisk a b)
  ; This HELP only for a, b is positive integer, and a > b.
  (define (help a b)
    (cond ((= b 0)   0)
          ((= b 1)   a)
          ((even? b) (help (double a) (halve b)))
          (else      (+ a (help a (- b 1))))))
  (cond ((< a 0) (- (asterisk (- a) b)))
        ((< b 0) (- (asterisk a (- b))))
        ((< a b) (asterisk b a))
        (else    (help a b))))

(asterisk 2 3)
(asterisk -2 3)
(asterisk 2 -3)
(asterisk -2 -3)
(asterisk 3 2)
(asterisk 0 3)
(asterisk 2 0)
(asterisk 0 0)
