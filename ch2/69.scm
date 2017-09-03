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

(define symbol-pair car)
(define weight-pair cadr)
(define (:.. f g h) (lambda (x) (f (g x) (h x))))
(define pair->leaf (:.. make-leaf symbol-pair weight-pair))
; build leaf-set and sort as weight order
(define (make-leaf-set pairs)
  (foldl (lambda (x acc)
           (adjoin-set (make-leaf (symbol-pair x) (weight-pair x))
                       acc))
         '()
         pairs))

(define (successive-merge leafs)
  (cond ((null? leafs) '())
        ((<= (length leafs) 1) (car leafs))
        (else (let ((x0 (car leafs))
                    (x1 (cadr leafs))
                    (xs (cddr leafs)))
                (successive-merge (adjoin-set (make-code-tree x0 x1) xs))))))

(define (compose f g) (lambda (x) (f (g x))))
; generate-huffmann-tree :: [(symbol, weight)] -> HT
(define generate-huffmann-tree
  (compose successive-merge make-leaf-set))

; My Code
; adjoin-set to merge pair to other pairs
; using CONS is wrong

; 2013-12-01 13:17:11
; shold reutrn (car pairs)
(define *pairs1* '((A 4) (B 2) (C 3) (D 1)))
(displayln *pairs1*)
(make-leaf-set *pairs1*)

(displayln (generate-huffmann-tree *pairs1*))

(define *pairs2* '((A 3) (B 3) (C 1) (D 1) (E 1) (F 1)))
(displayln (generate-huffmann-tree *pairs2*))

(define *pairs3* '((A 3)))
(displayln (generate-huffmann-tree *pairs3*))

(define *pairs4* '())
(displayln (generate-huffmann-tree *pairs4*))
