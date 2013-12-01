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

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
       (if (leaf? next-branch)
         ; if leaf? recur call to cons answer
         (cons (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
         (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
; simplily return left or right branch
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit" bit))))

(define *sample-tree*
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))

(display *sample-tree*)
(newline)

(define *sample-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode *sample-message* *sample-tree*))
(newline)
