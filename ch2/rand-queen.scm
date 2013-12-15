(define (displayln x) (display x) (newline) x)

(define (myfold proc init lst)
  (if (null? lst) init
    (proc (car lst) (myfold proc init (cdr lst)))))

; 去除列表中相等的多余元素,保证所有元素只出现一次
(define (make-set lst)
  (if (null? lst) '()
    (cons (car lst) (filter (lambda (x) (not (= (car lst) x)))
                            (make-set (cdr lst))))))

; 生成[low:1:high)的序列
(define (range low high)
  (if (>= low high) '()
    (cons low (range (+ low 1) high))))

; 交换lst的两个位
(define (swap-two! lst n1 n2)
  (if (= n1 n2) lst
  (let ((v (list-ref lst n1)))
   (begin (list-set! lst n1 (list-ref lst n2))
          (list-set! lst n2 v)))))

; 对序列进行随机化
(define (shuffle! lst)
  (cond ((= 1 (length lst)) lst)
        (else (begin (shuffle! (cdr lst))
                     (swap-two! lst 0 (random (length lst)))))))

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

; 评价一个冲突组合,这里使用冲突组元素个数的平方的和
(define (eval-group cg)
  (define (square x) (* x x))
  (apply + (map (lambda (x) (square (length x))) cg)))
; 生成列表中的两个随机数.
; 之类保证这两个数是不相等的
(define (get-rand-two lst)
  (let* ((len (length lst))
         (idx (random len))
         (idx1 (random (- len 1))))
   (cons (list-ref lst idx)
         ;将第一个随机数交换到最后,从而保证不重复 
         (begin (swap-two! lst idx (- len 1))
                (list-ref lst idx1)))))

; 生成一个随机交换对
; 注意这里是偶那个get-rand-two和make-set保证交换的两个数是不相等的
(define (get-rand-pair cg)
  (get-rand-two (make-set (myfold append '() cg))))

; 随机化方法增强序列(减少evel-group函数评价)
; 成功,则返回good.不能够减少返回bad,并且将序列交换回去
; 如果已经达到最小(也就是满足N-皇后的要)求
; 则返回为ok
(define (impore-lst lst)
  (let ((cg (conflict-gourp lst)))
   (let ((sel-pair (get-rand-pair cg)))
    (begin (swap-two! lst (car sel-pair) (cdr sel-pair))
           (let ((newcg (conflict-gourp lst)))
            (let ((cg-val (eval-group cg))
                  (newcg-val (eval-group newcg)))
              (cond ((= 0 newcg-val) 'ok)
                    ((< newcg-val cg-val) 'good)
                    (else (begin (swap-two! lst (car sel-pair) (cdr sel-pair))
                          'bad)))))))))
; 暴力随机生成一个N皇后的可行解.
; 注意,这样的随机方法可能会陷入到一个局部的最优点.
; 例如(0 8 5 11 1 3 10 6 2 4 9 7)
; 冲突序列为((7 9) (9 10))
; 但是交换其中的任意两个,都不能减少冲突
; 当尝试*max-iter*次数不能够减少冲突,则对序列进行随机
; 再次进行随驾算法
; TODO:
; 添加一个随机后的优化算法
(define (rand-q n)
  (next-loop (range 0 n)))

(define (next-loop lst)
  (if (loop lst 0) lst
    (begin 
           (shuffle! lst)
           (next-loop lst))))
(define (loop lst cnt)
  (define *max-iter* (* 10 (length lst)))
  (if (> cnt *max-iter*) #f
    (let ((ret (impore-lst lst)))
     (cond ((equal? 'ok ret) #t)
           (else (loop lst (+ cnt 1)))))))
(display  (rand-q 16))
(newline)
