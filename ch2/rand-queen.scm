; 生成[low:1:high)的序列
(define (range low high)
  (if (>= low high) '()
    (cons low (range (+ low 1) high))))

; 设置list的第n个数为v(从0开始计算索引)
(define (list-set! lst n v)
  (if (= n 0) (set-car! lst v)
    (cons (car lst) (list-set! (cdr lst) (- n 1) v))))

; 交换lst的两个位
(define (swap-two lst n1 n2)
  (if (= n1 n2) lst
  (let ((v (list-ref lst n1)))
   (begin (list-set! lst n1 (list-ref lst n2))
          (list-set! lst n2 v)))))

; 对序列进行随机化
(define (shuffle lst)
  (cond ((= 1 (length lst)) lst)
        (else (begin (shuffle (cdr lst))
                     (swap-two lst 0 (random (length lst)))))))

; 生成冲突序列
; (5 4 0 2 3 1)
; ((3 . 4) (1 . 3) (0 . 3) (0 . 1))
; 注意到0,1,3这个形成一个环,也就是任意选择其中的两个,都会冲突.如果交换其中的任意两个.可以减少冲突,但是不能够彻底减少冲突为0.
; 而如果直接交换一个冲突对的两个位置,则可能无法减少冲突.
(define (conflict-seq lst)
  (define (confict-one idx head idx2 tail ret)
    (cond ((null? tail) ret)
          ((= (- idx2 idx) (abs (- head (car tail))))
           (confict-one idx head (+ idx2 1) (cdr tail)
                        (cons (cons idx idx2) ret)))
          (else
            (confict-one idx head (+ idx2 1) (cdr tail) ret))))
  (define (helper idx head tail ret)
    (if (null? tail) ret
      (helper (+ 1 idx) (car tail) (cdr tail)
              (append (confict-one idx head (+ idx 1) tail '()) ret))))
  (helper 0 (car lst) (cdr lst) '()))

; 暴力随机生成一个N皇后的可行解.
; TODO:
; 添加一个随机后的优化算法
(define (rand-q n)
  (loop (range 0 n)))
(define (loop lst)
  (begin (shuffle lst)
         (let ((conf (conflict-seq lst)))
          (if (not (= 0 (length conf)))
            (loop lst)
            lst))
         ))
(display  (rand-q 15))
(newline)
