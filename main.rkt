#lang racket/base
;; This file is part of data-frame
;; Copyright (c) 2018 Alex Harsányi <AlexHarsanyi@gmail.com>

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require "private/df.rkt"
         "private/exn.rkt"
         "private/series.rkt"
         "private/describe.rkt"
         "private/statistics.rkt"
         "private/sql.rkt"
         "private/csv.rkt"
         "private/histogram.rkt"
         "private/meanmax.rkt"
         "private/scatter.rkt"
         "private/least-squares-fit.rkt")

(provide (all-from-out "private/df.rkt")
         (all-from-out "private/series.rkt")
         (all-from-out "private/exn.rkt")
         (all-from-out "private/describe.rkt")
         (all-from-out "private/statistics.rkt")
         (all-from-out "private/csv.rkt")
         (all-from-out "private/sql.rkt")
         (all-from-out "private/histogram.rkt")
         (all-from-out "private/meanmax.rkt")
         (all-from-out "private/scatter.rkt")
         (all-from-out "private/least-squares-fit.rkt"))

;; raco setup --check-pkg-deps --pkgs data-frame
;; raco test --no-run-if-absent --package data-frame

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
