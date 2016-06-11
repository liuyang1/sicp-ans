#lang racket

(define (my-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(list 'a 'b 'c)
(list (list 'google))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(my-memq 'red '((red shoes) (blue socks)))
(my-memq 'red '(red shoes blue socks))
