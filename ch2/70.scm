#lang racket
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf obj) (cadr obj))
(define (weight-leaf obj) (caddr obj))

; Huffmann tree struct
; (left-child right-child symbols-of-all-node weight-of-all-node)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; build leaf-set and sort as weight order
(define (make-leaf-set pairs)
  (if (null? pairs) '()
    (let ((pair (car pairs)))
     (adjoin-set (make-leaf (car pair)
                            (cadr pair))
                 (make-leaf-set (cdr pairs))))))

(define (generate-huffmann-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; My Code
; adjoin-set to merge pair to other pairs
; using CONS is wrong
(define (successive-merge pairs)
  (if (<= (length pairs) 1) (car pairs)
    (successive-merge
      (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                  (cdr (cdr pairs))))))

(define *pairs* '((a 2) (na 16) (boom 1) (sha 3)
                        (get 2) (yip 9) (job 2) (wah 1)))
(displayln *pairs*)

(define *tree* (generate-huffmann-tree *pairs*))
(displayln *tree*)

; encode message by tree
(define (encode message tree)
  (if (null? message) '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
; encode symbol by tree
(define (encode-symbol unit tree)
  ; check symbol is in symbol-lst
  ; this should replace by system internal function
  (define (in-list? unit lst)
    (cond ((null? lst) #f)
          ((equal? unit (car lst)) #t)
          (else (in-list? unit (cdr lst)))))
  ; check symbol is in tree
  (define (in? unit tree)
    (in-list? unit (symbols tree)))
  (cond ((leaf? tree) '())
        ((in? unit (left-branch tree)) (cons 0 (encode-symbol unit (left-branch tree))))
        ((in? unit (right-branch tree)) (cons 1 (encode-symbol unit (right-branch tree))))
        (else (error "not include tree" unit tree))))

(define *msg* '(get a job
                    sha na na na na na na na na
                    get a job
                    sha na na na na na na na na
                    wah yip yip yip yip yip yip yip yip yip
                    sha boom))
(displayln *msg*)
(displayln (length *msg*))
(define *encode-msg* (encode *msg* *tree*))
(displayln *encode-msg*)
(displayln (length *encode-msg*))
