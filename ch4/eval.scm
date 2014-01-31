(define (displayln x) (display x) (newline) x)
; exer 4.2
(define (myeval expr env)
  (cond ((self-evaluating? expr)    expr)
        ((variable? expr)           (lookup-variable-value expr env))
        ((quoted? expr)             (text-of-quotation expr))
        ((assignment? expr)         (eval-assignment expr env))
        ((definition? expr)         (eval-definition expr env))
        ((if? expr)                 (make-procedure
                                      (lambda-parameters expr)
                                      (lambda-body expr)
                                      env))
        ((begin? expr)              (eval-sequence (begin-actions expr) env))
        ((cond? expr)               (myeval (cond->if expr) env))
        ((application? expr)        (myapply (myeval (operator expr) env)
                                             (list-of-value (operands expr) env)))
        (else                       (error "Unkown expr type -- MYEVAL" expr))))

(define (myapply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-env (procedure-parameters procedure)
                                    arguments
                                    (procedure-env procedure))))
        (else
          (error "Unknown procedure type -- MYAPPLY" procedure))))

;;;
(define (list-of-value-origin exps env)
  (if (no-operands? exps) '()
    (cons (myeval (first-operand exps) env)
          (list-of-value-origin (rest-operands exps) env))))

; exer 4.1
; using LET, setting calc sequence
(define (list-of-value-left-right exps env)
  (if (no-operands? exps) '()
    (let ((left (myeval (first-operand exps) env)))
     (display left) (newline)
     (cons left
           (list-of-value-left-right (rest-operands exps) env)))))

(define (list-of-value-right-left exps env)
  (if (no-operands? exps) '()
    (let ((right (list-of-value-right-left (rest-operands exps) env)))
     (display right) (newline)
     (cons (myeval (first-operand exps) env)
           right))))

(define list-of-value list-of-value-left-right)

;;;
(define (eval-if expr env)
  (if (= #t (myeval (if-predicate expr) env))
    (myeval (if-consequent expr) env)
    (myeval (if-alternative expr) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (myeval (first-exp exps) env))
        (else (myeval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (myeval (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (myeval (definition-value expr) env)
                    env)
  'ok)

; 4.1.2
(define (self-evaluating? expr)
  (cond ((number? expr) #t)
        ((string? expr) #t)
        (else #f)))

(define (variable? expr) (symbol? expr))

; (quote TEXT-OF-QUOTATION)
(define (quoted? expr) (tagged-list? expr 'quote))
(define (text-of-quotation expr) (cadr expr))
(define (tagged-list? expr tag)
  (if (pair? expr) (eq? (car expr) tag)
    #f))

; (set! VAR VALUE)
(define (assignment? expr) (tagged-list? expr 'set!))
(define (assignment-variable expr) (cadr expr))
(define (assignment-value expr) (caddr expr))

; (define VAR VALUE)
; (define (VAR PARAM1 PARAM...) BODY)
; ->
; (define VAR (lambda (PARA1 PARAM...) BODY))
(define (definition? expr) (tagged-list? expr 'define))
(define (definition-variable expr)
  (if (symbol? (cadr expr))
    (cadr expr)         ; (define VAR VALUE)
    (caadr expr))       ; (define (VAR PARAM...) BODY)
  )
(define (definition-value expr)
  (if (symbol? (cadr expr))
    (caddr expr)
    (make-lambda (cdadr expr) (cddr exp))))

; lambda
; (lambda (lambda-parameters lambda-body))
(define (lambda? expr) (tagged-list? expr 'lambda))
(define (lambda-parameters expr) (cadr expr))
(define (lambda-body expr) (cddr expr))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; if
; (if if-predicate if-consequent if-alternative)
(define (if? expr) (tagged-list? expr 'if))
(define (if-predicate expr) (cadr expr))
(define (if-consequent expr) (caddr expr))
(define (if-alternative expr) (if (not (null? (cdddr expr))) (cadddr expr) #f))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
; (begin exp...)
(define (begin? expr) (tagged-list? expr 'begin))
(define (begin-actions expr) (cdr expr))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->expr seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? expr) (pair? expr))
(define (operator expr) (car expr))
(define (operands expr) (cdr expr))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond
(define (cond? expr) (tagged-list? expr 'cond))
(define (cond-clauses expr) (cdr expr))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if expr)
  (expand-clauses (cond-clauses expr)))
(define (expand-clauses clauses)
  (if (null? clauses) #f
    (let ((first (car clauses)) (rest (cdr clauses)))
     (if (cond-else-clause? first)
       (if (null? rest) (sequence->expr (cond-actions first))
         (error "ELSE clause isn't last -- COND->IF" clauses))
       (make-if (cond-predicate first)
                (sequence->expr (cond-actions first))
                (expand-clauses rest))))))


; 4.1.3
(define (true? x) (not (eq? x #f)))
(define (false? x) (eq? x #f))

; procedure
(define (make-procedure params body env)
  (list 'procedure params body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-env p) (cadddr p))

; environment
(define (enclosing-env env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-env '())
(define (make-frame vars vals)
  (cons vars vals))
(define (frame-var frame) (car frame))
(define (frame-val frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-env vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-env)
      (error "Unbound var" var)
      (let ((frame (first-frame env)))
       (scan (frame-var frame) (frame-val frame)))))
  (env-loop env))

; var in env
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-env env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-env)
      (error "Unbound var -- SET!" var)
      (let ((frame (first-frame env)))
       (scan (frame-var frame) (frame-val frame)))))
  (env-loop env))

; when env is THE-EMPTY-ENV, frame <= (first-frame env) is error
(define (define-variable! var val env)
  (if (eq? env the-empty-env)
    (extend-env (list var) (list val) env)
    (let ((frame (first-frame env)))
     (define (scan vars vals)
       (cond ((null? vars)
              (add-binding-to-frame! var val frame))
             ((eq? var (car vars))
              (set-car! vals val))
             (else
               (scan (cdr vars) (cdr vals)))))
     (scan (frame-var frame) (frame-val frame)))))

; 4.1.4
(define primitive-procedures
  (list (list 'car car) (list 'cdr cdr) (list 'cons cons) (list 'null? null?)
        ; others
        ))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (setup-env)
  (let ((initial-env (extend-env (primitive-procedure-names)
                                 (primitive-procedure-objects)
                                 the-empty-env)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define *global-env* (setup-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define (apply-in-underlying-scheme primitive-proc args)
  (display primitive-proc) (display " ") (display args)
  (newline))
