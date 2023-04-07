#lang racket/base

;; annealing.rkt --probabilistic find of an optimum goal.
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018, 2022, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/contract
         racket/math
         racket/random
         math/distributions)

;; Default temperature function for the annealing function.  As R grows from 0
;; to 1, the returned temperature needs to drop from 1 to 0.  The default
;; implementation is linear, but `exp` or `log` could also be used.
(define (default-temperature r)
  (- 1.0 r))

;; Default transition probability function as described in the simulated
;; annealing wikipedia page.
;;
;; NOTE: we return 1.1 to mean "always transition" since this guarantees that
;; the probability will be greater than what `random` returns.  Similarily, we
;; return -0.1 for "never transition" since this is always smaller than what
;; `random` returns.
(define (default-transition cost ncost temperature)
  (cond ((< ncost cost) 1.1)          ; always transition towards a lower cost
        ((> temperature 0.0)
         ;; NOTE: exponent is negative, since ncost > cost.  Also, we
         ;; normalize the exponent by `ncost`, ensuring that the "cost" part
         ;; is between 0 and 1, regardless of how big or small the costs are.
         (let* ((exponent (/ (- cost ncost) (* ncost temperature)))
                (probability (exp exponent)))
           probability))
        (#t -0.1)                       ; never transition if TEMPERATURE is 0
        ))

;; Default progress reporting function for the annealing function.  Does
;; nothing.
(define (default-progress _event _t _cost _state)
  (void))

;; Implement the simulated annealing algorithm as described in:
;; https://en.wikipedia.org/wiki/Simulated_annealing
;;
;; NOTE: this is a probabilistic goal finding function.  Running it twice with
;; the same inputs will produce a different output!
;;
;; INITIAL-STATE is the initial state of the system
;;
;; NEIGHBOUR is a function (STATE, TEMPERATURE) => NSTATE, given a state and
;; the temperature t, will return a new state.  Note that this function must
;; create a new state, the existing state should not be modified.
;;
;; GOAL is a function (STATE) => COST, that evaluates the cost of the state
;; (smaller is better).  STATE must not be modified by this function.
;;
;; PROGRESS-CALLBACK is a function of (EVENT TEMPERATURE COST STATE) => void,
;; it is used to report progress of the annealing search.  EVENT is one of
;; 'progress (periodic progress is being made), 'best-state (a new best state
;; was found), 'restart-search (search was restarted, see RESTART-AFTER),
;; 'final-state the final state has been reached.  TEMPERATURE, COST and STATE
;; are the corresponding values when the event is reported.
;;
;; **HINT** you can use continuations (or escape-continuations) in the
;; PROGRESS-CALLBACK to exit an annealing search early, if needed, for
;; example, to exit a long running search when the user presses Cancel in a
;; GUI
;;
;; TEMPERATURE is a function (iteration-pct) => temperature, that gets passed
;; in the percentage of the completed iterations
;; (num-iterations/max-iterations) and produces a "temperature" value.
;; Default function is ok for most use cases.
;;
;; TRANSITION is a function (cost, new-cost, temperature) => probability, that
;; computes the probability of transitioning to a new state given the cost of
;; the states and the temperature.  Default function is ok for most use cases.
;;
;; ITERATIONS is the number of iterations to perform
;;
;; RESTART-AFTER is the number of iteration steps that we search for an
;; improved solution compared to the current overall best.  When this count is
;; exceeded, we revert to the current overall best solution, and resume the
;; search from that point (at the current temperature).  The counter is reset
;; each time a new overall best is found.  This can be #f, in that case the
;; search is never restarted.  A value greater than the number of iterations
;; would also cause the search to never be restarted.
;;
;; The function returns two values: a goal state which minimizes the GOAL
;; function as well as the cost of the state.
;;
(define (annealing
         #:initial initial-state
         #:neighbour neighbour
         #:goal goal
         #:progress-callback (progress default-progress)
         #:temperature (temperature default-temperature)
         #:transition (transition default-transition)
         #:iterations (iterations 1000)
         #:restart-after (restart-after #f))

  (define initial-cost (goal initial-state))
  (define progress-steps (exact-truncate (/ iterations 100)))

  (for/fold (;; Current state and associated cost from where we continue the
             ;; search
             [state initial-state]
             [cost initial-cost]
             ;; Keep track of the "all time" best state and associated cost,
             ;; while we do the annealing search. This is what we return to
             ;; the user when we're done, and we also restart from this state
             ;; if the search is not making progress (see restart counter)
             [best-state initial-state]
             [best-cost initial-cost]
             ;; Number of steps until we restart the search from the
             ;; `best-state` -- this is reset each time a new best state is
             ;; found.
             [restart-counter restart-after]
             ;; Steps until we report progress via the callback.
             [progress-counter progress-steps]
             #:result
             (begin
               (progress 'final-state 0.0 best-cost best-state)
               (values best-state best-cost)))

            ([k (in-range 1 (add1 iterations))])

    (let* ((t (temperature (exact->inexact (/ k iterations))))
           (nstate (neighbour state t))
           (ncost (goal nstate))
           (p (transition cost ncost t)))
      (define updated-progress-counter
        (if (> progress-counter 0)
            (sub1 progress-counter)
            (begin
              (progress 'progress t ncost nstate)
              progress-steps)))
      (define-values (bstate bcost updated-restart-counter)
        (if (< ncost best-cost)
            (begin
              ;; New best state, save it and reset the restart counter
              (progress 'best-state t ncost nstate)
              (values nstate ncost restart-after))
            (values best-state best-cost (and restart-counter (sub1 restart-counter)))))
      (cond ((and restart-counter (< restart-counter 0))
             ;; This search has failed to find a better state after
             ;; "resart-after" steps. Return to the best state and continue
             ;; from there
             (progress 'restart-search t bcost bstate)
             (values bstate bcost bstate bcost restart-after updated-progress-counter))
            ((>= p (random))
             (values nstate ncost bstate bcost updated-restart-counter updated-progress-counter))
            (#t
             (values state cost bstate bcost updated-restart-counter updated-progress-counter))))))


;;;; ANNEALING PARAMETERS AND THEIR TRANSITION FUNCTIONS
;;
;; This section provides some helper functions for the `annealing` function to
;; construct the state from a single variable or multiple variables.  They are
;; useful in constructing a state and a transition function.  They don't have
;; to be used with the `annealing` function, but they make the use more
;; convenient.

;; "metadata" about an annealing parameter -- things that don't change when
;; transitioning the parameter: name, the type of distribution (normal or
;; uniform), initial value, min/max range and standard deviation, for normal
;; distributions.
(struct ameta (name dkind initial minimum maximum stddev) #:transparent)

;; An annealing parameter, together with the `transition-aparam` function can
;; be used to create "states" for the annealing function.  A parameter
;; encapsulates the current value, the distribution from which to sample other
;; values and metadata (see `ameta`).  Note that the distribution has a mean
;; of 0.
(struct aparam (value meta distribution) #:transparent)

;; Create a distribution from which to sample values for an annealing
;; parameter.  The distribution range is reduced by the TEMPERATURE parameter.
;; Note that the distribution has a mean of 0.
(define (make-distribution meta [temperature 1.0])
  (case (ameta-dkind meta)
    ((normal)
     (normal-dist 0 (* (ameta-stddev meta) temperature)))
    ((uniform)
     (let ([half-range (* 0.5 (- (ameta-maximum meta) (ameta-minimum meta)))])
       (uniform-dist 0 (- half-range) half-range)))
    (else
     ;; This should not happen, as there is a contract on make-aparam...
     (error (format "make-distribution: unknown distribution type: ~a" (ameta-dkind meta))))))

;; Create an annealing parameter -- this can be used by itself as the state
;; for the `annealing` function, or combined into more complex states
;; (possibly with other parameters)
(define (make-aparam name
                     #:min minimum
                     #:max maximum
                     #:initial [initial (/ (+ minimum maximum) 2)]
                     #:distribution-kind [dkind 'normal]
                     #:stddev [stddev (/ (- maximum minimum) 6)])
  (when (>= minimum maximum)
    (error "make-aparam: bad minimum maximum parameters"))
  (define meta (ameta name dkind initial minimum maximum stddev))
  (aparam initial meta (make-distribution meta)))

;; Transition an `aparam` according to the annealing rules and TEMPERATURE --
;; a new value is sampled from the distribution and a new aparam is
;; constructed from it.  This function is suitable to be passed as a
;; "neighbour" function to the `annealing` function when the state is a single
;; `aparam`
(define (transition-aparam p temperature)
  (define meta (aparam-meta p))
  (let loop ([nvalue (+ (aparam-value p) (sample (aparam-distribution p)))]
             [tries 10])
    (cond [(and (> nvalue (ameta-minimum meta))
                (< nvalue (ameta-maximum meta)))
           ;; Sample value is inside valid range, so we can use it
           (struct-copy
            aparam
            p
            [value nvalue]
            [distribution (make-distribution (aparam-meta p) temperature)])]
          [(> tries 0)
           ;; Sample value is out if range, try getting another one
           (loop (+ (aparam-value p) (sample (aparam-distribution p)))
                 (sub1 tries))]
          [#t
           ;; No more tries left, clamp to min or max
           (struct-copy
            aparam
            p
            [value (max (ameta-minimum meta) (min (ameta-maximum meta) nvalue))]
            [distribution (make-distribution (aparam-meta p) temperature)])])))

;; Transition a group of parameters according to annealing conventions.  GROUP
;; is a hash table of `aparam` instances and a single value is randomly
;; selected from the hash, transitioned using `transition-aparam`, and an
;; updated hash is returned as the new state.  This function is suitable to be
;; passed as a "neighbour" functin to the `annealing` function when the state
;; is a hash table of `aparam`s.
(define (transition-aparam-group group temperature)
  (define key (random-ref (hash-keys group)))
  (define nval (transition-aparam (hash-ref group key) temperature))
  (hash-set group key nval))

(provide
 (struct-out aparam)
 (struct-out ameta))

(provide/contract
 (annealing
  (parametric->/c
   (state)
   (->* (#:initial state
         #:neighbour (-> state (between/c 0 1) state)
         #:goal (-> state real?))
        (#:progress-callback (-> (or/c 'progress 'best-state 'restart-search 'final-state)
                                 (between/c 0 1)
                                 real?
                                 state
                                 any/c)
         #:temperature (-> (between/c 0 1) (between/c 0 1))
         #:transition (-> real? real? real? (between/c 0 1))
         #:iterations exact-nonnegative-integer?
         #:restart-after (or/c #f exact-nonnegative-integer?))
        (values state real?))))
 (make-aparam
  (->* ((or/c string? symbol?) #:min real? #:max real?)
       (#:initial real?
        #:distribution-kind (or/c 'normal 'uniform)
        #:stddev positive?)
       aparam?))
 (transition-aparam
  (-> aparam? (between/c 0 1) aparam?))
 (transition-aparam-group
  (-> (hash/c (or/c string? symbol?) aparam?)
      (between/c 0 1)
      (hash/c (or/c string? symbol?) aparam?))))
