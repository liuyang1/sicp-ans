(load "wire.scm")

(define (two-adder a1 a2 b1 b2 c-in s1 s2 c-out)
  (define c1 (make-wire))
  (full-adder a1 b1 c-in s1 c1)
  (full-adder a2 b2 c1 s2 c-out)
  'ok)

; test code
(define a1 (make-wire))
(define b1 (make-wire))
(define a2 (make-wire))
(define b2 (make-wire))
(define c-in (make-wire))
(define s1 (make-wire))
(define s2 (make-wire))
(define c-out (make-wire))

(two-adder a1 a2 b1 b2 c-in s1 s2 c-out)

(set-signal! a1 1)
(set-signal! a2 1)
(set-signal! b1 1)
(set-signal! b2 1)
(set-signal! c-in 1)
(display (append c-out s2 s1))
