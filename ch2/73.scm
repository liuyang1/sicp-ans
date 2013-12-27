; add put & get Proc
(load "oriData.scm")

; basic
(define (variable? e) (symbol? e))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))
(define (=number? expr num)
  (and (number? expr) (= expr num)))

; add
(define add-symbol '+)
(define (make-sum a1 . a2)
  (define (helper a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list add-symbol a1 a2))))
  (if (null? (cdr a2))
    (helper a1 (car a2))
    (make-sum a1 (make-sum (car a2) (cdr a2)))))
(define (sum? x) (and (pair? x) (eq? (car x) add-symbol)))
(define (addend s) (car s))
(define (augend s) (cadr s))

; multiply
(define product-symbol '*)
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list product-symbol m1 m2))))
(define (product? x) (and (pair? x) (eq? (car x) product-symbol)))
(define (multiplier p) (car p))
(define (multiplicand p) (cadr p))

; exponentiation
(define expo-symbol '^)
(define (make-expo base expo)
  (cond ((=number? expo 0) 1)
        ((=number? base 1) 1)
        ((=number? expo 1) base)
        (else (list expo-symbol base expo))))
(define (exponentiation? x) (and (pair? x) (eq? (car x) expo-symbol)))
(define (base expr) (car expr))
(define (expo expr) (cadr expr))

;;; a)
;;; add number? and same-variable? to func table
;;; because this not exist operator of number of var,
;;; so can NOT determine which func should to be call

;;; b)
(define (sum-deriv expr var)
  (make-sum (deriv (addend expr) var)
            (deriv (augend expr) var)))
(define (prod-deriv expr var)
  (make-sum
    (make-product (multiplicand expr)
                  (deriv (multiplier expr) var))
    (make-product (multiplier expr)
                  (deriv (multiplicand expr) var))))

(put 'deriv add-symbol sum-deriv)
(put 'deriv product-symbol prod-deriv)

;;; c)
(define (expo-deriv expr var)
  (make-product (make-product (expo expr)
                              (make-expo (base expr) (- (expo expr) 1)))
                (deriv (base expr) var)))
(put 'deriv expo-symbol expo-deriv)

;;; d)
;;; just modify as below
; (put add-symbol 'deriv sum-deriv)
;;; ...

;;; 2.73 example code
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;; test code
(displayln (deriv 'x 'x))
(displayln (deriv '(+ x 1) 'x))
(displayln (deriv '(* 3 x) 'x))
(displayln (deriv '(+ 1 (* 3 x)) 'x))
(displayln (deriv '(^ x 3) 'x))
