#lang racket

; data Tree = entry (Tree left) (Tree right)
;           | nil

(provide (all-defined-out))

(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (entry tree) (car tree))
(define (make-tree entry left right)
  (list entry left right))

; middle sequence, recursive style
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

; middle sequence, iterate style
(define (tree->list-2 tree)
  (define (copy-to-list tree res)
    (if (null? tree) res
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        res)))))
  (copy-to-list tree '()))

; 打印一支树
; 按照tree这个命令的样子
(define (disp tree)
  ; 打印前缀的最后引导符号
  (define (end)
    (display "`-- "))
  ; 打印前缀
  (define (disPreix prefix)
    (if (= (length prefix) 1)
      (display "`-- ")
      (if (null? prefix) #t
        (if (eq? (car prefix) 'l)
          ; 打印标示层级结构
          (begin (display "|   ") (disPreix (cdr prefix)))
          (begin (display "    ") (disPreix (cdr prefix)))))))
  ; 打印树的所有孩子
  (define (showlist tree prefix)
    (if (null? (cdr tree))
      ; 打印最后一个孩子
      (hlp (car tree) (append prefix ( list 'r)))
      (begin (hlp (car tree) (append prefix (list 'l)))
             (showlist (cdr tree) prefix))))
  ; 打印树
  (define (hlp tree prefix)
    (begin (disPreix prefix)
           (if (null? tree)
             (displayln "end")
             (begin (displayln (entry tree))
                    (showlist (cdr tree) prefix)))))
  (hlp tree '()))


(define *tree* '(52 (555 (32 () ()) ())
                 (189 (42 () (8111 () ()))
                  ())))

(define *tree1* '(7 (3 (1 () ()) (5 () ()))
                  (9 () (11 () ()))))
(define *tree2* '(3 (1 () ())
                  (7 (5 () ()) (9 () (11 () ())))))
(define *tree3* '(5 (3 (1 () ()) ())
                  (9 (7 () ()) (11 () ()))))
(define *tree-case* (list *tree1* *tree2* *tree3* *tree*))

(define (test-case tree->list)
  (for-each (lambda (t) (displayln (tree->list t))) *tree-case*))

; (for-each disp *tree-case*)
; (test-case tree->list-1)
; (test-case tree->list-2)

; answer:
; a) same.
; b) same.
