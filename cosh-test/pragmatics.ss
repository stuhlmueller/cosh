#!r6rs

(library

 (cosh-test pragmatics)

 (export pragmatics-expr)

 (import (rnrs))

 (define pragmatics-expr
   '(
     
     (define force
       (lambda (obj)
         (if (pair? obj)
             (if (eq? (first obj) 'delayed)
                 (force ((rest obj)))
                 obj)
             obj)))

     (define mark-delayed
       (lambda (thunk)
         (pair 'delayed thunk)))


     ;;scalar implicature exs

     ;;use partial knowledge form of belief formation.
     ;;for this to make sense the state must be the state of each object (rather than the total number true).
     ;;(define (belief actual-state access) (lambda () (map (lambda (ac st pr) (if ac st (pr))) access actual-state substate-priors)))
     (define belief
       (lambda (actual-state access)
         (map (lambda (ac st pr) (if ac st (sample pr)))
              access
              actual-state
              (substate-priors))))

     (define baserate
       (lambda () 0.6))

     (define substate-priors
       (lambda ()
         (list (lambda () (flip (baserate)))
               (lambda () (flip (baserate)))
               (lambda () (flip (baserate))))))

     (define state-prior
       (lambda () (map sample (substate-priors))))

     ;;use truth-functional meanings for sentences.
     (define sentence-prior
       (lambda () (uniform-draw (list all-p some-p none-p))))

     (define all-p
       (lambda (state) (all state)))
     (define some-p
       (lambda (state) (any state)))
     (define none-p
       (lambda (state) (not (some-p state))))

     ;;what is the speaker likely to say, given their informational access and an assumed state of the world?
     (define speaker
       (lambda (access state depth)
         (rejection-query
          (lambda ()
            ((lambda (s)
               (pair
                (equal? (belief (force state) access) (listener access s depth))
                (lambda () (force s))))
             (mark-delayed sentence-prior))))))

     ;;what state of teh world will the listener infer, given what the speaker said and the speaker's informational access?
     (define listener
       (lambda (speaker-access sentence depth)
         (rejection-query
          (lambda ()
            ((lambda (state)
               (pair
                (if (= 0 depth)
                    ((force sentence) (force state)) ;;sentence is true of state.
                    (equal? (force sentence)
                            (speaker speaker-access state (- depth 1))) ;;sentence is what speaker would have said given state and access.
                    )
                (lambda () (force state))))
             (mark-delayed state-prior))))))

     ;;(define (random-access access N) (if (= 0 num) ...))
     (define num-true
       (lambda (state) (sum (map (lambda (x) (if x 1 0)) state))))

     (num-true (force (listener '(#t #t #t) some-p 2)))

     ))

 )