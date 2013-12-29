(load "wire.scm")
(define (or-gate a1 a2 output)
  (define or-gate-delay 1)
  (define (or-action)
    (let ((new-val (logical-or (get-signal a1) (get-signal a2))))
     (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)

; test code
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))

(or-gate a b c)

(set-signal! a 0)
(set-signal! b 0)
(displayln (get-signal c))

(set-signal! a 1)
(set-signal! b 0)
(displayln (get-signal c))

(set-signal! a 0)
(set-signal! b 1)
(displayln (get-signal c))

(set-signal! a 1)
(set-signal! b 1)
(displayln (get-signal c))
