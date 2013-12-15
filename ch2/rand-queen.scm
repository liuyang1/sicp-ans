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

; 新的冲突序列生成方法
; (5 4 0 2 3 1)
; ((3 4) (0 1 3))
; 明显这样是一个更为合理的结果.
; 这种方法可以直接检查序列与其索引的和/差,然后将重复的提取出来.
; 和: (5 5 2 5 7 6) -> (0 1 3)
; 差: (5 3 -2 -1 -1 -4) -> (3 4)

; 构成和/差序列,主要利用index过程,对其进行索引化
(define (conflict-gourp lst)
  (define (index lst)
     (map cons (range 0 (length lst)) lst))
  (let ((idx (range 0 (length lst))))
  (append (group-index (index (map + lst idx)))
          (group-index (index (map - lst idx))))))

; 对索引化后的序列,进行检查.
; 将具有相同的和/差值的索引聚合起来
(define (group-index lst)
  (define (helper head tail ret)
    (let ((same-index (filter (lambda (v) (= (cdr v) (cdr head))) tail)) 
          (diff-index (filter (lambda (v) (not (= (cdr v) (cdr head)))) tail)))
      (let ((newret (append ret (list (cons (car head) (map car same-index))))))
       ; 若剩余的序列长度小于等于1,则退出.
       ; 退出,则筛选序列组长度大于1(也就是该组成员不只1个)的
       (if (<= (length diff-index) 1)
         (filter (lambda (x) (> (length x) 1)) newret)
         (helper (car diff-index) (cdr diff-index) newret)))))
  (helper (car lst) (cdr lst) '()))

; 暴力随机生成一个N皇后的可行解.
; TODO:
; 添加一个随机后的优化算法
(define (rand-q n)
  (loop (range 0 n)))
(define (loop lst)
  (begin (shuffle lst)
         (let ((conf (conflict-gourp lst)))
          (if (not (= 0 (length conf)))
            (loop lst)
            lst))
         ))
(display  (rand-q 8))
(newline)
