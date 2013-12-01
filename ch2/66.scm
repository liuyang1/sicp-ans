; define binary-tree set basic function
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (entry tree) (car tree))

; define get key of record funciton
; this simpily define key equal to record
(define (key record) record)

; define binary-tree set lookup function
; return record or #f if not found
(define (lookup given-key tree-set)
  (cond ((null? tree-set) #f)
        ((> given-key (key (entry tree-set))) (lookup given-key (right-branch tree-set)))
        ((< given-key (key (entry tree-set))) (lookup given-key (left-branch tree-set)))
        (else (entry tree-set))))

(define *tree-set* '(3 (2 () ()) (5 (4 () ()) (7 () ())) ))
(display *tree-set*)
(newline)
(display (lookup 4 *tree-set*))
(newline)
(display (lookup 1 *tree-set*))
(newline)
