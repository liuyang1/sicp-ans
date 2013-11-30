(define (displayln entry)
  (display entry)
  (newline))
(define (extract-entry game ass-list)
  (let ((entry (car ass-list)))
   (if (equal? game (car entry)) entry
     (extract-entry game (cdr ass-list))))) 
;; note that you will need to write extract-entry

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)
;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;	    
(define (play-loop-3 strat0 strat1 strat2)
  (define (play-loop-iter count hist0 hist1 hist2 limit)
    (if (= count limit)
      (begin 
        (displayln (get-probability-of-c (make-history-summary hist0 hist1 hist2)))
        (print-out-results-3 hist0 hist1 hist2 limit))
      (let ((res0 (strat0 hist0 hist1 hist2))
            (res1 (strat1 hist1 hist2 hist0))
            (res2 (strat2 hist2 hist0 hist1)))
        (play-loop-iter (+ count 1)
                        (extend-history res0 hist0)
                        (extend-history res1 hist1)
                        (extend-history res2 hist2)
                        limit))))
  (play-loop-iter 0 the-empty-history the-empty-history the-empty-history
                  (+ 90 (random 21))))

(define (print-out-results-3 hist0 hist1 hist2 num)
  (let ((scores (average-score (get-scores-3 hist0 hist1 hist2) num)))
   ; (display "Player 1: ")
   ; (displayln (list-ref scores 0))
   ; (display "Player 2: ")
   ; (displayln (list-ref scores 1))
   ; (display "Player 3: ")
   ; (displayln (list-ref scores 2))
   scores))

(define (average-score scores num)
  (map (lambda (score) (* 1.0 (/ score num))) scores))

(define (get-scores-3 hist0 hist1 hist2)
  (define (helper hist0 hist1 hist2 score0 score1 score2)
    (if (empty-history? hist0)
      (list score0 score1 score2)
      (let ((game (make-play (most-recent-play hist0)
                             (most-recent-play hist1)
                             (most-recent-play hist2))))
        (helper (rest-of-plays hist0)
                (rest-of-plays hist1)
                (rest-of-plays hist2)
                (+ (get-player-points-3 0 game) score0)
                (+ (get-player-points-3 1 game) score1)
                (+ (get-player-points-3 2 game) score2)))))
  (helper hist0 hist1 hist2 0 0 0))
(define *game-association-list-3*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

(define (get-player-points-3 num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list-3*)))

;; STRAT-list

; three-player strats
(define (NASTY-3 myhist hist1 hist2) "d")
(define (PASTY-3 myhist hist1 hist2) "c")
(define (SPASTIC-3 myhist hist1 hist2)
  (if (= (random 2) 0) "c" "d"))

(define (tough-Eye-for-Eye myhist hist1 hist2)
  (if (empty-history? myhist) "c"
    (if (or (equal? (most-recent-play hist1) "d")
            (equal? (most-recent-play hist2) "d"))
      "d"
      "c")))

(define (soft-Eye-for-Eye myhist hist1 hist2)
  (if (empty-history? myhist) "c"
    (if (and (equal? (most-recent-play hist1) "d")
             (equal? (most-recent-play hist2) "d"))
      "d"
      "c")))

; combine strat
(define (tough-combine r1 r2)
  (if (or (string=? r1 "d") (string=? r2 "d")) "d" "c"))
(define (soft-combine r1 r2)
  (if (and (string=? r1 "d") (string=? r2 "d")) "d" "c"))
(define (rand-combine r1 r2)
  (if (= (random 2) 0) r1 r2))

(define (make-combined-strategies two-player-strat0 two-player-strat1 combine)
  (lambda (myhist hist1 hist2)
    (combine (two-player-strat0 myhist hist1)
             (two-player-strat1 myhist hist2))))

; two-player start
(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))
(define (EGALITARIAN my-history other-history)
  (define (majority-loop cs ds hist)
    (cond ((empty-history? hist) (if (> ds cs) "d" "c"))
          ((string=? (most-recent-play hist) "c")
           (majority-loop (+ 1 cs) ds (rest-of-plays hist)))
          (else
            (majority-loop cs (+ 1 ds) (rest-of-plays hist)))))
  (majority-loop 0 0 other-history))

(define tough-Eye-for-Eye2
  (make-combined-strategies EYE-FOR-EYE EYE-FOR-EYE tough-combine))
(define soft-Eye-for-Eye2
  (make-combined-strategies EYE-FOR-EYE EYE-FOR-EYE soft-combine))
(define rand-eye-and-ega
  (make-combined-strategies EYE-FOR-EYE EGALITARIAN rand-combine))
(define tough-eye-and-ega
  (make-combined-strategies EYE-FOR-EYE EGALITARIAN tough-combine))

;; PLAY framework
(define (add-list lst0 lst1)
  (if (or (null? lst0) (null? lst1)) '()
    (cons (+ (car lst0) (car lst1))
          (add-list (cdr lst0) (cdr lst1)))))
(define (play-one-one-list strat0 strat1 list2 scores)
  (if (null? list2)
    (begin ;(displayln strat0) (displayln strat1) (displayln scores)
           scores)
    (let ((strat2 (car list2)))
     (begin ;(displayln strat0) (displayln strat1) (displayln strat2)
            (let ((new-scores(play-loop-3 strat0 strat1 strat2)))
             (play-one-one-list strat0 strat1 (cdr list2)
                                (add-list scores new-scores)))))))
(define empty-scores (list 0 0 0))
(define (play-one-list-list strat0 list1 list2 scores)
  (if (null? list1)
    (begin (displayln strat0) (displayln scores)
           scores)
    (let ((new-scores (play-one-one-list strat0 (car list1) list2 empty-scores)))
           (play-one-list-list strat0 (cdr list1) list2
                               (add-list new-scores scores)))))
(define (play-list-list-list list0 list1 list2)
  (if (null? list0) '()
    (begin (play-one-list-list (car list0) list1 list2 empty-scores)
           (play-list-list-list (cdr list0) list1 list2))))
(define (play-test-all strat-list)
  (play-list-list-list strat-list strat-list strat-list))
(define (play-test-one start0 strat-list)
  (play-one-list-list start0 strat-list strat-list empty-scores))

(define *start-list*
  (list NASTY-3 PASTY-3 SPASTIC-3 tough-Eye-for-Eye soft-Eye-for-Eye))
; (play-test-one NASTY-3 *start-list*)
; (play-test-one PASTY-3 *start-list*)
; (play-test-one PASTY-3 *start-list*)
; tough-Eye-for-Eye BEST
; (play-test-one tough-Eye-for-Eye *start-list*)
; (play-test-one soft-Eye-for-Eye *start-list*)

; (play-test-one tough-Eye-for-Eye2 *start-list*)
; (play-test-one soft-Eye-for-Eye2 *start-list*)
; (play-test-one tough-eye-and-ega *start-list*)

; summary -> c c -> c d total
;         -> c d -> c d total
;         -> d d -> c d total

(define empty-summary-entry (list 0 0 0))
(define empty-summary (list empty-summary-entry empty-summary-entry empty-summary-entry))

(define (update-summary-entry entry s0)
  (if (string=? s0 "c")
    (list (+ 1 (car entry)) (cadr entry) (+ 1 (caddr entry)))
    (list (car entry) (+ 1 (cadr entry)) (+ 1 (caddr entry)))))
(define (update-summary summary s1 s2 s0)
  (cond ((and (string=? s1 "c") (string=? s2 "c"))
         (list (update-summary-entry (car summary) s0)
               (cadr summary) (caddr summary)))
        ((and (string=? s1 "d") (string=? s2 "d"))
         (list (car summary) (cadr summary)
               (update-summary-entry (caddr summary) s0)))
        (else (list (car summary)
                    (update-summary-entry (cadr summary) s0)
                    (caddr summary)))))
(define (make-history-summary myhist0 hist1 hist2)
  (define (loop myhist0 hist1 hist2 summary)
    (if (<= (length myhist0) 1) summary
      (let ((s1 (cadr hist1))
            (s2 (cadr hist2))
            (s0 (car myhist0)))
        (loop (cdr myhist0) (cdr hist1) (cdr hist2)
              (update-summary summary s1 s2 s0)))))
  (loop myhist0 hist1 hist2 empty-summary))

(define (get-probability-of-c summary)
  (map (lambda (entry)
         (if (= 0 (caddr entry)) '() (/ (car entry) (caddr entry))))
       summary))
(define (get-probability-of-d summary)
  (map (lambda (entry)
         (if (= 0 (caddr entry)) '() (/ (cadr entry) (caddr entry))))
       summary))
; test for make-history-summary
; (define summary (make-history-summary
;                   (list "c" "c" "d" "d" "c" "d" "c" "c")
;                   (list "c" "c" "c" "d" "d" "c" "d" "c")
;                   (list "c" "c" "d" "d" "d" "c" "c" "c")))
; (define new-summary (make-history-summary
;                       (list "c" "c" "c" "d" "c")
;                       (list "d" "c" "d" "d" "c")
;                       (list "d" "c" "c" "c" "c")))
; (displayln new-summary)
; (displayln (get-probability-of-c new-summary))
; (displayln (get-probability-of-d new-summary))
(play-loop-3 soft-Eye-for-Eye SPASTIC-3 SPASTIC-3)
(play-loop-3 tough-Eye-for-Eye SPASTIC-3 SPASTIC-3)
(play-loop-3 PASTY-3 SPASTIC-3 SPASTIC-3)
(play-loop-3 NASTY-3 SPASTIC-3 SPASTIC-3)
(play-loop-3 SPASTIC-3 SPASTIC-3 SPASTIC-3)
;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f))) 

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))
; get-probability-of-c
; (1 1 0) soft-Eye-for-Eye
; (1 0 0) tough-Eye-for-Eye
; (1 1 1) fool(PASTY-3)
; (0 0 0) NASTY-3
; (1/2 1/2 1/2) SPASTIC-3
(define (dont-tolerate-fools myhist hist1 hist2)
  (if (< (length myhist) 10) "c"
    (if (and (could-he-be-a-fool? hist1 myhist hist2)
             (could-he-be-a-fool? hist2 myhist hist1))
      "d"
      "c")))

(play-loop-3 NASTY-3 PASTY-3 PASTY-3)
(play-loop-3 dont-tolerate-fools PASTY-3 PASTY-3)

(displayln "Finally finish Project2 Prisoner's Dillema")
(displayln "liuyang1@mail.ustc.edu.cn")
