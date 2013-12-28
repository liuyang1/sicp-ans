(define (displayln x) (display x) (newline) x)

(define (lookup tbl kl)
  (cond ((null? tbl) #f)
        ((eq? (car kl) (caar tbl))
         (if (null? (cdr kl)) (cdar tbl)
           (lookup (cdar tbl) (cdr kl))))
        (else (lookup (cdr tbl) kl))))

(define *tbl* '((i . ((am . liu))) (you . ((are . sam)))))
(displayln (lookup *tbl* '(i am)))
(displayln (lookup *tbl* '(i are)))
(displayln (lookup *tbl* '(you are)))

; TODO
; (displayln (insert! *tbl1* '(i) 'liu))
; (displayln (insert! *tbl1* '(i) 'are))
; (displayln (insert! *tbl1* '(you are) 'sam))
