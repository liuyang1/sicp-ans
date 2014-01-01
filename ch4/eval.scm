(define (myeval expr env)
  (cond ((self-evaluating? expr)    expr)
        ((variable? expr)           (lookup-variable-value expr env))
        ((quoted? expr)             (text-of-quotation expr))
        ((assignment? expr)         (eval-assignment? expr env))
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
(define (list-of-value exps env)
  (if (no-operands? exps) '()
    (cons (myeval (first-operand exps) env)
          (list-of-value (rest-operands exps) env))))

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
