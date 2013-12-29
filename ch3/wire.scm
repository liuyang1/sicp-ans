(use-modules (srfi srfi-18))
(define (displayln x) (display x) (newline) x)

(define (make-wire)
  (define val 0)
  (define action (lambda () 'default-action))
  (cons val action))

(define (get-signal wire)
  (car wire))
(define (set-signal! wire v)
  (set-car! wire v)
  ((cdr wire))
  )
(define (add-action! wire no-arg-proc)
  (set-cdr! wire no-arg-proc))

(define (inverter input output)
  (define inverter-delay 1)
  (define (invert-input)
    (let ((new-val (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! input invert-input)
  'ok)

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
