(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ a1 a2))
           me)
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- sum a1))
           me)
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- sum a2))
           me)))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-a-value) (process-forget-value))
          (else (error "ADDER unkown request" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-value constraint) (constraint 'I-have-a-value))
(define (inform-no-value constraint) (constraint 'I-lost-a-value))

(define (make-connector)
  (let ((value #f) (informant #f) (constraint '()))
   (define (set-my-value newval setter)
     (cond ((not (has-value? me)) (set! value newval)
                                  (set! informant setter)
                                  (for-each-except setter
                                                   inform-value
                                                   constraint))
           ((not (= value newval)) (error "Contradiction" value newval))))
   (define (forget-my-value retractor)
     (if (eq? retractor informant) (begin (set! informant #f)
                                          (for-each-except retractor
                                                           inform-no-value
                                                           constraint))
       'ignored))
   (define (connect new-cons)
     (if (not (memq new-cons constraint))
       (set! constraint (cons new-cons constraint)))
     (if (has-value? me) (inform-value new-cons)))
   (define (me request)
     (cond ((eq? request 'has-value?) (if informant #t #f))
           ((eq? request 'value) value)
           ((eq? request 'set-value!) set-my-value)
           ((eq? request 'forget) forget-my-value)
           ((eq? request 'connect) connect)
           (else (error "CONNECTOR Unkown request" request))))
   me))
