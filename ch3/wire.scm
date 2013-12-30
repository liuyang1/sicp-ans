(use-modules (srfi srfi-18))
(define (displayln x) (display x) (newline) x)

(define (make-wire)
  (define val 0)
  (define actionlst '()) ; default action list is null
  (cons val actionlst))

(define (get-signal wire)
  (car wire))
(define (get-actionlist wire)
  (cdr wire))
(define (set-signal! wire v)
  (set-car! wire v)
  (for-each (lambda (x) (x)) (get-actionlist wire))
  )
(define (add-action! wire no-arg-proc)
  (set-cdr! wire (cons no-arg-proc (get-actionlist wire))))

(define (inverter input output)
  (define inverter-delay 0)
  (define (invert-input)
    (let ((new-val (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! input invert-input)
  'ok)

; TODO:
; how to simulate time-sequence???
(define (after-delay dt proc)
  (sleep dt)
  (proc))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal" s))))
(define (logical-or s0 s1)
  (cond ((and (= s0 0) (= s1 0)) 0)
        ((or (= s0 1) (= s1 1)) 1)
        (else (error "invalid signal" s0 s1))))
(define (logical-and s0 s1)
  (cond ((and (= s0 1) (= s1 1)) 1)
        ((or (= s0 0) (= s1 0)) 0)
        (else (error "invalid signal" s0 s1))))

(define (and-gate a1 a2 output)
  (define and-gate-delay 0)
  (define (and-action)
    (let ((new-val (logical-and (get-signal a1) (get-signal a2))))
     (after-delay and-gate-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! a1 and-action)
  (add-action! a2 and-action)
  'ok)

(define (or-gate a1 a2 output)
  (define or-gate-delay 0)
  (define (or-action)
    (let ((new-val (logical-or (get-signal a1) (get-signal a2))))
     (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
   (and-gate a b c)
   (or-gate a b d)
   (inverter c e)
   (and-gate d e s)
   'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
   (half-adder b c-in s c1)
   (half-adder a s sum c2)
   (or-gate c1 c2 c-out)
   'ok))

; test code
; (define a (make-wire))
; (define b (make-wire))
; (inverter a b)
; 
; (set-signal! a 1)
; (displayln (get-signal b))
; 
; (set-signal! a 0)
; (displayln (get-signal b))

; test AND-gate
;(define a (make-wire))
;(define b (make-wire))
;(define c (make-wire))
;
;(and-gate a b c)
;
;(set-signal! a 0)
;(set-signal! b 0)
;(displayln (get-signal c))
;
;(set-signal! a 1)
;(set-signal! b 0)
;(displayln (get-signal c))
;
;(set-signal! a 0)
;(set-signal! b 1)
;(displayln (get-signal c))
;
;(set-signal! a 1)
;(set-signal! b 1)
;(displayln (get-signal c))
