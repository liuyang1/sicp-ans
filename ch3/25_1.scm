(define (make-table same-key?)
  (let ((records '()))
   (define (insert! k v)
     (let ((ret (lookup k)))
      (if (eq? ret #f)
        (set! records (cons (cons k v) records))
        (begin (set-car! ret k) (set-cdr! ret v)))))
   (define (lookup k)
     (define (hlp k rd)
       (cond ((null? rd) #f)
             ((same-key? k (caar rd)) (car rd))
             (else (hlp k (cdr rd)))))
     (hlp k records))
   (define (mydisplay)
     (display records) (newline))
   (define (dispatch m) 
     (cond ((eq? m 'lookup) lookup)
           ((eq? m 'insert!) insert!) 
           ((eq? m 'display) mydisplay)
           (else (error "unkown method" m)))) 
   dispatch))

; test code
(define (appr-same? x0 x1)
  (eq? x0 x1))

(define *t* (make-table appr-same?))
((*t* 'insert!) '1 'a)
((*t* 'display))
((*t* 'insert!) '(1 2 3) 'a)
((*t* 'display))
((*t* 'insert!) '(1 2 5) 'b)
((*t* 'display))
((*t* 'insert!) '(1 2 3) 'b)
((*t* 'display))