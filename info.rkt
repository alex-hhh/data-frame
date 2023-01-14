#lang info
(define collection "data-frame")
(define license 'LGPL-3.0-or-later)
(define pkg-desc "A Data Frame Implementation for Racket")
(define version "0.0")
(define pkg-authors '(AlexHarsanyi@gmail.com))
(define deps '("db-lib"
               "draw-lib"
               "math-lib"
               "plot-gui-lib"
               "plot-lib"
               "srfi-lite-lib"
               "typed-racket-lib"
               "rackunit-lib"
               "base"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "db-doc"
                     "math-doc"
                     "plot-doc"
                     "al2-test-runner"))
(define scribblings '(("scribblings/data-frame.scrbl" ())))
