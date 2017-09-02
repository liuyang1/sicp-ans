#lang racket

; basic
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? expr num)
  (and (number? expr) (= expr num)))

;;; for midfix expression
(define (build-expr symbol v1 v2) (list v1 symbol v2))
(define symbol cadr)
(define fst-operand car)
(define snd-operand caddr)
; use rst-operand instead snd-operand to work for '(x + y + z) expression
(define (rst-operand s)
  (if (= 3 (length s))
    (snd-operand s)
    (cddr s)))

; add
(define add-symbol '+)
(define (make-sum a1 . a2)
  (define (helper a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (build-expr add-symbol a1 a2))))
  (if (null? (cdr a2))
    (helper a1 (car a2))
    (make-sum a1 (make-sum (car a2) (cdr a2)))))
(define (sum? x) (and (pair? x) (eq? (symbol x) add-symbol)))
(define addend fst-operand)
(define augend rst-operand)

; multiply
(define product-symbol '*)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (build-expr product-symbol m1 m2))))
(define (product? x) (and (pair? x) (eq? (symbol x) product-symbol)))
(define multiplier fst-operand)
(define multiplicand rst-operand)

;;; 56. add exponentiation
(define expo-symbol '^)
(define (make-expo base expo)
  (cond ((=number? expo 0) 1)
        ((=number? base 1) 1)
        ((=number? expo 1) base)
        (else (build-expr expo-symbol base expo))))
(define (exponentiation? x) (and (pair? x) (eq? (symbol x) expo-symbol)))
(define base fst-operand)
(define expo snd-operand)

(define (deriv expr var)
  (cond ((number? expr)     0)
        ((variable? expr)   (if (same-variable? expr var) 1 0))
        ((sum? expr)        (make-sum (deriv (addend expr) var)
                                      (deriv (augend expr) var)))
        ((product? expr)    (make-sum
                              (make-product (multiplicand expr)
                                            (deriv (multiplier expr) var))
                              (make-product (multiplier expr)
                                            (deriv (multiplicand expr) var))))
        ((exponentiation? expr)
         (make-product
           (make-product (expo expr)
                         (make-expo (base expr) (- (expo expr) 1)))
           (deriv (base expr) var)))
        (else (error "unknown expression type --DERIV" expr))))

(define (unit-test sample expect)
  (let ((ret (deriv sample 'x)))
   (display (format "deriv(~a) = ~a should be ~a~n" sample ret expect))))

(unit-test '(x + y) 1)
(unit-test '((x * y) * (x + 3)) '(((2 * x) + 3) * y))
(unit-test '(x * x) '(2 * x))
(unit-test '(x + (y + (x + 3))) 2)
(unit-test '(x * y) 'y)

(unit-test '(x + x + x) 3)
(unit-test '(x * y * (x + 3)) '(((2 * x) + 3) * y))
