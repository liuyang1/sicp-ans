; leaf struct
; ('leaf SYMBOL WEIGHT)
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

; encode message by tree
(define (encode message tree)
  (if (null? message) '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))
; encode symbol by tree
(define (encode-symbol unit tree)
  ; check symbol is in tree
  (define (in? unit tree)
    (member unit (symbols tree)))
  (cond ((leaf? tree) '())
        ((in? unit (left-branch tree)) (cons 0 (encode-symbol unit (left-branch tree))))
        ((in? unit (right-branch tree)) (cons 1 (encode-symbol unit (right-branch tree))))
        (else (error "not include tree" unit tree))))


(define *sample-tree*
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(display *sample-tree*)
(newline)

(define *sample-message* '(A D A B B C A))
(display *sample-message*)
(newline)
(display (encode *sample-message* *sample-tree*))
(newline)
