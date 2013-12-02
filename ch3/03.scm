(define (make-account balance init-pwd)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; must add a method here
  ; because dispatch should return a proc, who has one arument
  (define (error-password amount)
    "Incorrect password")
  (define (dispatch pwd m)
    (cond ((not (eq? pwd init-pwd)) error-password); first check password
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "unkown request" m))))
  dispatch)

; test code
(define acc (make-account 100 'password))
(display ((acc 'password 'withdraw) 10))
(newline)
(display ((acc 'someother 'deposit) 50))
(newline)
