#lang racket
(define (double a) (* a 2))
(define (halve a) (/ a 2))


(define (asterisk a b)
  (define (ip m a b)
    (cond ((< a 0)      (- (ip m (- a) b)))
          ((< b 0)      (- (ip m a (- b))))
          ((= b 0)      m)
          ((even? b)    (ip m (double a) (halve b)))
          (else         (ip (+ m a) a (- b 1))))) 
  (ip 0 a b))
; keep (a * b + m) is not change

; test
(asterisk 2 3)
(asterisk -2 3)
(asterisk 2 -3)
(asterisk -2 -3)
(asterisk 3 2)
(asterisk 0 3)
(asterisk 2 0)
(asterisk 0 0)
