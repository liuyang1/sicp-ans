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

; this should not need to do this.
; because this have so much repeating calc.
(define (disp-pascal n)
  (define (helper i)
    (if (< i n)
      (begin  (display (pascal-line i)) 
              (newline)
              (helper (+ i 1)))
      #t))
  (helper 0))

(disp-pascal 10)
