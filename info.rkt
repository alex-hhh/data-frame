#lang info
(define collection "data-frame")
(define deps '("db-lib"
               "draw-lib"
               "math-lib"
               "plot-gui-lib"
               "plot-lib"
               "srfi-lite-lib"
               "typed-racket-lib"
               "rackunit-lib"
               "base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "db-doc" "math-doc" "plot-doc"))
(define scribblings '(("scribblings/data-frame.scrbl" ())))
(define pkg-desc "Data Frame")
(define version "0.0")
(define pkg-authors '(aharsanyi))
