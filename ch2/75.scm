(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
            (error "Unkown op -- MAKE-FROM_REAL-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

; test code
(define *1+i* (make-from-mag-ang 1 (/ 3.14 6)))

(define (displayln x) (display x) (newline) x)

(displayln (apply-generic 'real-part *1+i*))
(displayln (apply-generic 'imag-part *1+i*))
(displayln (apply-generic 'magnitude *1+i*))
(displayln (apply-generic 'angle *1+i*))
