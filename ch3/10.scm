(define-syntax mylet
  (syntax-rules () ((mylet ((VAR EXPR)) BODY)
                    ((lambda (VAR) BODY) EXPR))))

(define (make-withdraw initial-amount)
  (mylet ((balance initial-amount))
         (lambda (amount)
           (if (>= balance amount)
             (begin (set! balance (- balance amount))
                    balance)
             "Insufficient funds"))))

(define W1 (make-withdraw 100))
(display (W1 50))
(newline)
(define W2 (make-withdraw 10))
