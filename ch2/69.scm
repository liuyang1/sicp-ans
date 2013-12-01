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

; 2013-12-01 13:17:11
; shold reutrn (car pairs)
(define (successive-merge pairs)
  (if (<= (length pairs) 1) (car pairs)
    (successive-merge
      (adjoin-set (make-code-tree (car pairs) (cadr pairs))
                  (cdr (cdr pairs))))))

(define *pairs* '((A 4) (B 2) (C 1) (D 1)))
(display *pairs*)
(newline)

(display (generate-huffmann-tree *pairs*))
(newline)

(define *pairs2* '((A 3) (B 3) (C 1) (D 1) (E 1) (F 1)))
(display (generate-huffmann-tree *pairs2*))
(newline)
