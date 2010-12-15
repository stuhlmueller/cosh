#!r6rs

(library

 (cosh-test pragmatics)

 (export pragmatics-expr)

 (import (rnrs))

 (define pragmatics-expr
   '(
     ;;scalar implicature exs

     ;;use partial knowledge form of belief formation.
     ;;for this to make sense the state must be the state of each object (rather than the total number true).
     ;;(define (belief actual-state access) (lambda () (map (lambda (ac st pr) (if ac st (pr))) access actual-state substate-priors)))
     (define (belief actual-state access)
       (map (lambda (ac st pr) (if ac st (sample pr)))
            access
            (force actual-state)
            (substate-priors)))

     (define (baserate) 0.6)
     
     (define (substate-priors)
       (list (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))
             (lambda () (flip (baserate)))))

     (define (state-prior)
       (map sample (substate-priors)))

     ;;use truth-functional meanings for sentences.
     (define (sentence-prior)
       (uniform-draw (list all-p some-p none-p)))

     (define (all-p state) (all (force state)))
     (define (some-p state) (any (force state)))
     (define (none-p state) (not (some-p (force state))))

     ;;what is the speaker likely to say, given their informational
     ;;access and an assumed state of the world?
     (define (speaker access state depth)
       (rejection-query
        (define s (delay (sentence-prior)))
        s
        (equal? (belief state access)
                (force (listener access s depth))))) 
     
     ;;what state of teh world will the listener infer, given what the
     ;;speaker said and the speaker's informational access?
     (define (listener speaker-access sentence depth)
       (rejection-query
        (define state (delay (state-prior)))
        state
        (if (= 0 depth)
            ((force sentence) state) ;;sentence is true of state.
            (equal? (force sentence) ;;sentence is what speaker would have said given state and access.
                    (force (speaker speaker-access state (- depth 1))))
            )))
     
     ;;(define (random-access access N) (if (= 0 num) ...))
     (define (num-true state)
       (sum (map (lambda (x) (if x 1 0)) state)))
     
     (num-true (force (listener '(#t #t #t) some-p 1)))

     ))

 )