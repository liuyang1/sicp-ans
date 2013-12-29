(load "wire.scm")

(define a (make-wire))
(define b (make-wire))
(define an (make-wire))
(define bn (make-wire))
(define c (make-wire))
(define cn (make-wire))

(inverter a an)
(inverter b bn)
(and-gate an bn cn)
(inverter cn c)

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
