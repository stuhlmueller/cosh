#!r6rs

(library

 (cosh-test pragmatics)

 (export pragmatics-expr)

 (import (rnrs))

 (define pragmatics-expr
   '(
     
     (define (force obj)
       (if (pair? obj)
           (if (eq? (first obj) 'delayed)
               (force ((rest obj)))
               obj)
           obj))

     (define (mark-delayed thunk)
       (pair 'delayed (mem thunk)))

     ;;scalar implicature exs

     ;;use partial knowledge form of belief formation.
     ;;for this to make sense the state must be the state of each object (rather than the total number true).
     ;;(define (belief actual-state access) (lambda () (map (lambda (ac st pr) (if ac st (pr))) access actual-state substate-priors)))
     (define (belief actual-state access)
       (map (lambda (ac st pr) (if ac st (sample pr)))
            access
            (force actual-state)
            (substate-priors)))

     (define baserate
       (lambda () 0.6))

     (define (substate-priors)
       (list (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))))

     (define (state-prior)
       (map sample (substate-priors)))

     ;;use truth-functional meanings for sentences.
     (define (sentence-prior)
       (uniform-draw (list all-p some-p none-p)))

     (define (all-p state) (all state))
     (define (some-p state) (any state))
     (define (none-p state) (not (some-p state)))

     ;;what is the speaker likely to say, given their informational access and an assumed state of the world?
     (define (speaker access state depth)
       (rejection-query
        (define s (mark-delayed sentence-prior))
        (force s)
        (equal? (belief state access) (listener access s depth))))
     
     ;;what state of teh world will the listener infer, given what the speaker said and the speaker's informational access?
     (define (listener speaker-access sentence depth)
       (rejection-query
        (define state (mark-delayed state-prior))
        (force state)
        (if (= 0 depth)
            ((force sentence) (force state)) ;;sentence is true of state.
            (equal? (force sentence)
                    (speaker speaker-access state (- depth 1))) ;;sentence is what speaker would have said given state and access.
            )))
     
     ;;(define (random-access access N) (if (= 0 num) ...))
     (define (num-true state)
       (sum (map (lambda (x) (if x 1 0)) state)))
     
     (num-true (force (listener '(#t #t #t) some-p 2)))

     ))

 )