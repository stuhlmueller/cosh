#!r6rs

;; Copied from Bher.

(library

 (cosh desugar)

 (export de-sugar
         de-sugar-all
         de-sugar-toplevel
         begin-wrap
         partition-begin
         register-sugar!
         register-query-sugar)

 (import (rnrs)
         (scheme-tools)
         (_srfi :1))
 
;;;some syntax utils
 (define (mem? sexpr) (tagged-list? sexpr 'mem))
 (define (lambda? exp) (tagged-list? exp 'lambda))
 (define (lambda-parameters exp) (cadr exp))
 (define (lambda-body exp) (caddr exp))
 (define (quoted? exp) (tagged-list? exp 'quote))
 (define (begin? exp) (tagged-list? exp 'begin))
 (define (definition? exp) (tagged-list? exp 'define))
 (define (if? exp) (tagged-list? exp 'if))
 (define (application? exp) (pair? exp))
 (define (letrec? exp) (tagged-list? exp 'letrec))

;;;include paths
 (define include-paths  (list "./" "include/" "./church/")) ;;FIXME: include scheme search-path?
 ;;(append (list "./" "include/") (map (lambda (search-path) (string-append search-path "/include/")) (search-paths))))

                                        ; goes through a list of library paths and opens
                                        ; the first one it finds
 (define (open-included-file filename)
   (define (loop-through-paths path-list)
     (if (null? path-list)
         (error "open-included-file" (string-append "File " filename " not found on Church include paths."))
         (if (file-exists? (string-append (first path-list) filename))
             (open-input-file (string-append (first path-list) filename))
             (loop-through-paths (rest path-list)))))
   (loop-through-paths include-paths))

;;;Gimme some sugar!

 ;;de-sugaring code:
 (define sugar-registry '())
 (define (register-sugar! pattern translator . times-to-try)
   (set! sugar-registry (cons (list pattern translator times-to-try) sugar-registry)) )
 (define sugar-pattern first)
 (define sugar-translator second)
 (define times-to-try third)

 (define (de-sugar expr)
   (define unchanged (gensym))
   (define (try expr sugar-list)
     (if (null? sugar-list)
         unchanged
         (if ((sugar-pattern (first sugar-list)) expr)
             ((sugar-translator (first sugar-list)) expr)
             (try expr (rest sugar-list)) )))
   (let loop ((expr expr)
              (pass 0))
     (let ((new-expr (try expr (filter (lambda (s) (or (null? (times-to-try s)) (< pass (first (times-to-try s))))) sugar-registry))))
       (if (eq? new-expr unchanged)
           expr
           (loop new-expr (+ pass 1)) ))))

 (define (de-sugar-all sexpr)
   (let ((new-sexpr (de-sugar sexpr)))
     (if (list? new-sexpr)
         (map de-sugar-all new-sexpr)
         new-sexpr)))

 ;; keep begin-defines at top level, desugar within
 (define (de-sugar-toplevel sexpr)
   (if (begin? sexpr)
       (let ([e (expand-loads sexpr)])
         (let-values ([(defines non-defines) (partition-begin e)])
           `(begin
              ,@(map de-sugar-all defines)
              ,(de-sugar-all (begin-wrap non-defines)))))
       (de-sugar-all sexpr)))

 ;; (begin ...)

 (define (begin-wrap exprs)
   (if (null? (rest exprs))
       (first exprs)
       `(begin ,@exprs)))

 ;; (begin ...) is now a special form!
                                        ;(define (desugar-begin expr)
                                        ;  (last expr))
                                        ;(register-sugar begin? desugar-begin)

 ;; (let (var-bindings) expr1 ... exprN)
 (define (let? expr) (and (tagged-list? expr 'let) (list? (second expr))))
 (define (let->lambda expr)
   (let* ((bindings (second expr))
          (vars (map first bindings))
          (value-exprs (map second bindings))
          (body (begin-wrap (drop expr 2))))
     `((lambda ,vars ,body) ,@value-exprs) ))

 ;; (let loop (var-bindings) expr1 ... exprN)

 (define (named-let? expr) (and (tagged-list? expr 'let) (symbol? (second expr))))

 (define (named-let->letrec expr)
   `(letrec ((,(second expr) (lambda ,(map first (third expr)) ,(begin-wrap (drop expr 3))))) (,(second expr) ,@(map second (third expr)))) )

 (define (named-let->lambda expr)
   (let* ((proc-name (second expr))
          (let-conversion (let->lambda (rest expr))))
     `((Y (lambda (,proc-name) ,(first let-conversion))) ,@(rest let-conversion)) ))

 ;; (let* ...)
 (define (let*? expr) (tagged-list? expr 'let*))
 (define (desugar-let* expr)
   (let ((bindings (second expr))
         (body (begin-wrap (drop expr 2))))
     (if (null? bindings)
         body
         (let* ((binding (first bindings))
                (var (first binding))
                (value-exprs (second binding)) )
           `((lambda (,var) (let* ,(rest bindings) ,body)) ,value-exprs) ))))

 ;; (case ...)
 (define (case? expr) (tagged-list? expr 'case))
 (define (desugar-case expr)
   (let ((key-symbol (gensym))
         (key-expr (second expr))
         (value-exprs (drop expr 2)) )
     `(let ((,key-symbol ,key-expr))
        (cond ,@(map (lambda (value-expr)
                       (let ((vals (first value-expr))
                             (val-expr (rest value-expr)) )
                         (cond ((list? vals)
                                `((any (list ,@(map (lambda (val) `(equal? ,key-symbol ,val)) vals) ))
                                  ,@val-expr ) )
                               ((equal? vals 'else)
                                `(else ,@val-expr) )
                               (else (error "Invalid case expression." vals)) ) ))
                     value-exprs ))) ))

 ;; (cond ...)
 (define (cond? expr) (tagged-list? expr 'cond))
 (define (desugar-cond expr)
   (let loop ((conditions (rest expr)))
     (if (null? conditions)
         '(void)
         (let* ((condition (first conditions))
                (test (first condition)))
           (if (equal? test 'else)
               (if (not (null? (rest conditions)))
                   (error expr "else clause in cond expression must be last.")
                   (begin-wrap (rest condition)) )
               `(if ,test
                    ,(begin-wrap (rest condition))
                    ,(loop (rest conditions)) ) )))))

 ;; (when ...)
 (define (when? expr) (tagged-list? expr 'when))
 (define (desugar-when expr)
   `(if ,(second expr)
        (begin
          ,@(cddr expr))
        (void)))

 ;;define sugar: (define (foo x y) ...)
 (define (define-fn? expr) (and (tagged-list? expr 'define) (not (symbol? (second expr)))))
 (define (desugar-define-fn expr)
   (if (define-fn? expr)
       (let ((def-var (first (second expr)))
             (def-params (rest (second expr)))
             (def-body (rest (rest expr))))
         `(define ,def-var (lambda ,def-params ,@def-body)))
       expr))

 ;;load sugar.
 (define (seq-with-load? expr) (and (list? expr)
                                    (fold (lambda (subexpr accum) (or (tagged-list? subexpr 'load) accum)) false expr)))
 (define (expand-loads expr)
   (apply append (map (lambda (subexpr) (if (load? subexpr) (file->list (open-included-file (second subexpr))) (list subexpr))) expr)))
 (define (file->list filehandle)
   (let ((next (read filehandle)))
     (if (eof-object? next) '() (cons next (file->list filehandle)))))
 (define (load? expr) (tagged-list? expr 'load))

 ;; desugar-define-fn here is to make defines be in estandard form.
 (define (partition-begin e)
   (let* ((defines (map desugar-define-fn (filter (lambda (e) (tagged-list? e 'define)) (rest e))))
          (non-defines (filter (lambda (e) (not (tagged-list? e 'define))) (rest e))))
     (values defines non-defines)))
   
 ;;we desugar (begin .. define ..) into letrec for this implementation.
 (define (begin-defines? sexpr)
   (and (tagged-list? sexpr 'begin) (not (null? (filter (lambda (e) (tagged-list? e 'define)) sexpr)))))
 (define (desugar-begin-defines sexpr)
   (let-values ([(defines non-defines) (partition-begin sexpr)])
     `(letrec ,(map rest defines) ,(begin-wrap non-defines))))

 ;;normal-form procedure returns a pair of condition-value and query-thunk. query thunk samples from the conditional predictive.
 ;; transforms into a def-query that expects church code of
 ;; form: (query-name arg1 arg2 ... (define ...)
 ;; ... query-expr condition-expr)
 ;;note: primitive-name shouldn't be the same as query-name, because otherwise desugarring doesn't know when to stop.
 (define (register-query-sugar query-name)
   (define (query? expr) (and (tagged-list? expr query-name)
                              (>= (length (rest expr)) 2))) ;;make sure not to try de-sugaring the definition of the query -- queries have at least two subexprs.
   (define (desugar-query expr)
     (let*-values ([ (control-part defs) (break (lambda (subexpr) (tagged-list? subexpr 'define)) (drop-right expr 2))]
                   [ (control-args) (rest control-part)]
                   [ (query-exp cond-exp) (apply values (take-right expr 2))])
       `(,query-name ,@control-args (lambda () (begin ,@defs (pair ,cond-exp (lambda () ,query-exp)))) )))
   (register-sugar! query? desugar-query 1))

 ;;psmc-query needs to be handled slightly differently, because the query code gets temps->nfqp which takes 'temperature' arguments then gives the nfqp.
 ;;assumes call form (psmc-query <temp-args> <temps> ..other-control-args.. ..defines.. <query-exp> <cond-exp>).
 (define (tempered-query? query-name expr)
   (and (tagged-list? expr query-name)
        (>= (length (rest expr)) 2))) ;;make sure not to try de-sugaring the definition of the query -- queries have at least two subexprs.
 (define (desugar-tempered-query query-name expr)
   (let*-values ([(control-part defs) (break (lambda (subexpr) (tagged-list? subexpr 'define)) (drop-right expr 2))]
                 [(temp-args) (second control-part)]
                 [(temps) (third control-part)]
                 [(control-args) (drop control-part 3)]
                 [(query-exp cond-exp) (apply values (take-right expr 2))])
     `(,query-name ,temps ,@control-args (lambda ,temp-args (lambda () (begin ,@defs (pair ,cond-exp (lambda () ,query-exp))))) )))
 
 (define (psmc-query? expr)
   (tempered-query? 'psmc-query expr))
 (define (desugar-psmc-query expr)
   (desugar-tempered-query 'psmc-query expr))
 
 (define (mh-query/annealed-init? expr)
   (tempered-query? 'mh-query/annealed-init expr))
 (define (desugar-mh-query/annealed-init expr)
   (desugar-tempered-query 'mh-query/annealed-init expr))

 ;;lazify adds delay to an expression. make sure that the expression is fully-desugarred first!
 (define (lazify? expr) (tagged-list? expr 'lazify))
 (define (desugar-lazify expr) (make-lazy (de-sugar-all (second expr))))
 (define (make-lazy sexpr)
   (cond
    ((or (begin? sexpr) (mem? sexpr)) (map make-lazy sexpr))
    ((quoted? sexpr) sexpr)
    ((letrec? sexpr) `(letrec ,(map (lambda (binding) (list (first binding) (delay-expr (second binding))))
                                    (second sexpr))
                        ,(make-lazy (third sexpr))))
    ((lambda? sexpr) `(lambda ,(lambda-parameters sexpr) ,(make-lazy (lambda-body sexpr)))) ;;delay body?
    ((if? sexpr) `(if ,(make-lazy (second sexpr)) ,(delay-expr (third sexpr)) ,(delay-expr (fourth sexpr))))
    ((application? sexpr) `(,(make-lazy (first sexpr)) ,@(map delay-expr (rest sexpr))))
    (else sexpr) ))
 (define (delay-expr sexpr)
   (if (or (lambda? sexpr) (and (mem? sexpr) (lambda? (first sexpr))))
       (make-lazy sexpr)
       `(pair 'delayed (mem (lambda () ,(make-lazy sexpr))))))

 (define (delay? expr) (tagged-list? expr 'delay))
 (define (desugar-delay expr) `(pair 'delayed (mem (lambda () ,(de-sugar-all (second expr))))))

 ;;do the fragment grammar thing to an arbitrary expression.
 ;;FIXME make alpha flexible..
 ;; (define (fragmentize? expr) (tagged-list? expr 'fragmentize))
 ;; (define (desugar-fragmentize expr) (make-fragment (de-sugar-all (second expr))))
 ;; (define (make-fragment sexpr)
 ;;   (cond
 ;;    ((or (begin? sexpr) (mem? sexpr)) (map make-fragment sexpr))
 ;;    ((quoted? sexpr) sexpr)
 ;;    ;((definition? sexpr) `(define ,(second sexpr)  ,(stochastic-delay-expr (third sexpr))))
 ;;    ((letrec? sexpr) `(letrec ,(map (lambda (binding) (list (first binding) (stochastic-delay-expr (second binding))))
 ;;                                    (second sexpr))
 ;;                        ,(make-fragment (third sexpr))))
 ;;    ((lambda? sexpr) `(DPmem 1.0 (lambda ,(lambda-parameters sexpr) ,(make-fragment (lambda-body sexpr)))))
 ;;    ((if? sexpr) `(if ,(make-fragment (second sexpr)) ,(stochastic-delay-expr (third sexpr)) ,(stochastic-delay-expr (fourth sexpr))))
 ;;    ((application? sexpr) `(,(make-fragment (first sexpr)) ,@(map stochastic-delay-expr (rest sexpr))))
 ;;    (else sexpr) ))
 ;; (define (stochastic-delay-expr sexpr)
 ;;   `(let ((de (list 'delayed (lambda () ,(make-fragment sexpr)))))
 ;;      (if (flip) de (force de))))

 (define (fragment-lambda? expr) (tagged-list? expr 'f-lambda))
 (define (desugar-fragment-lambda sexpr)
   `(DPmem 1.0 (lambda ,(lambda-parameters sexpr) (if (flip) ,(lambda-body sexpr) (list 'delayed (lambda () ,(lambda-body sexpr)))))))
   
 (register-sugar! fragment-lambda? desugar-fragment-lambda)
 (register-sugar! lazify? desugar-lazify)
 ;(register-sugar! fragmentize? desugar-fragmentize)


                                        ; @form (let ((var val) ...) expr ...)
                                        ; @desc
                                        ; Let binds variables in the scope of the body of the let.
                                        ; @param assignments An expression '((var val) ...)
                                        ; @param exprs Body expressions that are evaluated within the environment where variables are assigned.
                                        ; @return the result of evaluating the last body expr
 (register-sugar! let? let->lambda)

                                        ; @form (let* ((var val) ...) expr ...)
                                        ; @desc
                                        ; Let* binds variables in the scope of the body of the let.
                                        ; Each assignment has access to the variables bound earlier on in the same let*.
                                        ; @param assignments An expression '((var val) ...)
                                        ; @param exprs Body expressions that are evaluated within the environment where variables are assigned.
                                        ; @return the result of evaluating the last body expr
 (register-sugar! let*? desugar-let*)

 (register-sugar! named-let? named-let->letrec)
 (register-sugar! case? desugar-case)
 (register-sugar! cond? desugar-cond)
 (register-sugar! begin-defines? desugar-begin-defines)
 (register-sugar! define-fn? desugar-define-fn)
 (register-sugar! seq-with-load? expand-loads)
 (register-sugar! when? desugar-when)

  ;;syntacic sugar query forms:
 (register-query-sugar 'mh-query)
 (register-query-sugar 'rejection-query)
 (register-query-sugar 'enumeration-query)
 ;(register-query-sugar 'primitive-laplace-mh-query 'laplace-mh-query)
 ;(register-query-sugar 'primitive-gradient-query 'gradient-query)

 (register-sugar! psmc-query? desugar-psmc-query 1)
 (register-sugar! mh-query/annealed-init? desugar-mh-query/annealed-init 1)

 (register-sugar! delay? desugar-delay)

 )
