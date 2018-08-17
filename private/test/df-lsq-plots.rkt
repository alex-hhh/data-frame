#lang racket/base

;; df-lsq-plots.rkt -- generate plots for the `df-least-squares-fit`
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require "../csv.rkt"
         "../df.rkt"
         "../least-squares-fit.rkt"
         plot
         racket/class
         racket/runtime-path)

(define-runtime-path test-file "./test-data/lsq-test.csv")
(define df (df-read/csv test-file))

;; This file is used to visually inspect the fit curves produced by
;; `df-least-squares-fit` for various fit methods.  Running each of the
;; functions below will display the plot in a separate frame.

(define (show-linear-plot)
  (define fit-linear (df-least-squares-fit
                      df "base" "linear"
                      #:mode 'linear #:residual? #t))
  (define plot-linear-frame
    (plot-frame
     (list (points (df-select* df "base" "linear"))
           (function fit-linear))
     #:title "linear fit"))

  (send plot-linear-frame show #t))

(define (show-second-plot)
  (define fit-second (df-least-squares-fit
                      df "base" "second"
                      #:mode 'polynomial
                      #:polynomial-degree 2
                      #:residual? #t))

  (define plot-second-frame
    (plot-frame
     (list (points (df-select* df "base" "second"))
           (function fit-second))
     #:title "second degree"))

  (send plot-second-frame show #t))

(define (show-third-plot)
  (define fit-third (df-least-squares-fit
                     df "base" "third"
                     #:mode 'polynomial
                     #:polynomial-degree 3
                     #:residual? #t))

  (define plot-third-frame
    (plot-frame
     (list (points (df-select* df "base" "third"))
           (function fit-third))
     #:title "third degree"))

  (send plot-third-frame show #t))

(define (show-exp-plot)
  (define fit-exp (df-least-squares-fit
                   df "base" "exp"
                   #:mode 'exponential
                   #:residual? #t
                   #:annealing? #t
                   #:annealing-iterations 1000))

  (define plot-exp-frame
    (plot-frame
     (list (points (df-select* df "base" "exp"))
           (function fit-exp))
     #:title "exponential"))

  (send plot-exp-frame show #t))

(define (show-log-plot)
  (define fit-log (df-least-squares-fit
                   df "base2" "log"
                   #:mode 'logarithmic
                   #:residual? #t))

  (define plot-log-frame
    (plot-frame
     (list (points (df-select* df "base2" "log"))
           (function fit-log))
     #:title "logarithmic"))
  (send plot-log-frame show #t))

(define (show-pow-plot)
  (define fit-pow (df-least-squares-fit
                   df "base2" "pow"
                   #:mode 'power
                   #:residual? #t
                   #:annealing? #t
                   #:annealing-iterations 1000))

  (define plot-pow-frame
    (plot-frame
     (list (points (df-select* df "base2" "pow"))
           (function fit-pow))
     #:title "power"))
  (send plot-pow-frame show #t))
