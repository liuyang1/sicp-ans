(define (displayln x) (display x) (newline) x)

(define (make-wire)
  (define default-in 'nothing)
  (define default-out 'nothing)
  (define action (lambda () 'default-action))
  (list default-in default-out action))

(define (get-signal wire)
  (list-ref wire 0))
(define (set-signal! wire v)
  (list-set! wire 0 v)
  (list-set! wire 1 ((list-ref wire 2)))
  )
(define (add-action! wire no-arg-proc)
  (list-set! wire 2 no-arg-proc))

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
