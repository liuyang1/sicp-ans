(load "oriData.scm")
(define (=zero-number? n)
  (eq? n 0))
(define (=zero-rational? n)
  (define (num n) (car n))
  (eq? (num n) 0))
(define (=zero-complex? n)
  (define real-part car)
  (define imag-part cdr)
  (and (eq? (real-part n) 0) (eq? (imag-part n) 0)))

(put '=zero? 'scheme-number =zero-number?)
(put '=zero? 'rational =zero-rational?)
(put '=zero? 'complex =zero-complex?)

; test code
(displayln ((get '=zero? 'scheme-number) 0))
(displayln ((get '=zero? 'rational) '(0 . 1)))
(displayln ((get '=zero? 'complex) '(0 . 0)))
