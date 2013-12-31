(define (displayln x) (display x) (newline) x)

;;; wire base
(define (make-wire)
  (define val 0)
  (define actionlst '()) ; default action list is null
  (cons val actionlst))

(define (get-signal wire)
  (car wire))
(define (get-actionlist wire)
  (cdr wire))
(define (set-signal! wire v)
  (if (not (= v (get-signal wire)))
    (begin (set-car! wire v)
           (for-each (lambda (x) (x)) (get-actionlist wire)))
    'done)
  )
(define (add-action! wire no-arg-proc)
  (set-cdr! wire (cons no-arg-proc (get-actionlist wire)))
  (no-arg-proc))        ; why runit? TODO:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (after-delay dt proc)
  (add-to-agenda! (+ dt (current-time *agenda*))
                  proc
                  *agenda*))

;;; basic logical
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; basic device
(define (inverter input output)
  (define inverter-delay 2)
  (define (invert-input)
    (let ((new-val (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define and-gate-delay 3)
  (define (and-action)
    (let ((new-val (logical-and (get-signal a1) (get-signal a2))))
     (after-delay and-gate-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! a1 and-action)
  (add-action! a2 and-action)
  'ok)

(define (or-gate a1 a2 output)
  (define or-gate-delay 5)
  (define (or-action)
    (let ((new-val (logical-or (get-signal a1) (get-signal a2))))
     (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-val)))))
  (add-action! a1 or-action)
  (add-action! a2 or-action)
  'ok)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; basic Arithmetic
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
   (or-gate a b d)          ; TODO: swap order, but get differenet ansewer!!!
   (and-gate a b c)
   (inverter c e)
   (and-gate d e s)
   'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
   (half-adder b c-in s c1)
   (half-adder a s sum c2)
   (or-gate c1 c2 c-out)
   'ok))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; agenda
(define (make-agenda)
  (let ((agenda '()))
   (define (empty?) (null? agenda))
   (define (first) (if (empty?) '() (car agenda)))
   (define (pop) (set! agenda (cdr agenda)))
   (define (add! time action)
     (set! agenda (sort (cons (cons time action) agenda) ; stable sort
                        (lambda (x y)
                          (< (car x) (car y))))))
   (define (disp) (for-each displayln agenda))
   (define (dispatch m)
     (cond ((eq? m 'empty?) empty?)
           ((eq? m 'first) first)
           ((eq? m 'pop) pop)
           ((eq? m 'add!) add!) 
           ((eq? m 'display) disp)
           (else (error "unkown method of agenda" m))))
   dispatch))

(define (empty-agenda? agenda) ((agenda 'empty?)))
(define (first-agenda-action agenda) (cdr ((agenda 'first))))
(define (current-time agenda) (if (empty-agenda? agenda) 0 (car ((agenda 'first)))))
(define (remove-first-agenda agenda) ((agenda 'pop)))
(define (add-to-agenda! time action agenda) ((agenda 'add!) time action))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (propagate)
  (if (empty-agenda? *agenda*) 'done
    (let ((first-item (first-agenda-action *agenda*)))
     (first-item)
     (remove-first-agenda *agenda*)
     (propagate))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (probe name wire)
  (add-action! wire (lambda ()
                      (display (current-time *agenda*)) (display " ")
                      (display name) (display " ")
                      (display (get-signal wire))
                      (newline))))

; test code
(define *agenda* (make-agenda))
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

((*agenda* 'display))
(propagate)
