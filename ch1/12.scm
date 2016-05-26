; recursive calc pascal triangle line
(define (pascal-line n)
  (define (add-two lst)
    (if (< (length lst) 2) '()
      (cons (+ (car lst) (cadr lst)) (add-two (cdr lst)))))
  (define (inc lst)
    (cons 1 (append (add-two lst) (list 1))))
  (cond ((= n 0) '(1))
        ((= n 1) '(1 1))
        (else (inc (pascal-line (- n 1))))))

(define (compose f g) (lambda (x) (f (g x))))
(map (compose displayln pascal-line) (range 15))
