#lang racket
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
  (if (null? tree-set)
    #f
    (let* ((v (entry tree-set))
           (k (key v)))
      (if (= given-key k)
        v
        (lookup given-key
                ((if (> given-key k) right-branch left-branch)
                 tree-set))))))

(define *tree-set* '(3 (2 () ()) (5 (4 () ()) (7 () ())) ))
(displayln *tree-set*)
(displayln (lookup 4 *tree-set*))
(displayln (lookup 1 *tree-set*))
