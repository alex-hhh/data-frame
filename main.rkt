#lang racket/base
;; This file is part of data-frame
;; Copyright (c) 2018, 2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>

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
         "private/least-squares-fit.rkt"
         "private/for-df.rkt")

(provide (all-from-out "private/series.rkt")
         (all-from-out "private/exn.rkt")
         (all-from-out "private/statistics.rkt")
         (all-from-out "private/csv.rkt")
         (all-from-out "private/sql.rkt")
         (all-from-out "private/histogram.rkt")
         (all-from-out "private/meanmax.rkt")
         (all-from-out "private/scatter.rkt")
         (all-from-out "private/least-squares-fit.rkt")
         (all-from-out "private/for-df.rkt"))

(provide
 data-frame?
 df-add-derived!
 df-add-index!
 df-add-index*!
 df-add-lazy!
 df-add-series!
 df-all-indices-of
 df-contains/any?
 df-contains?
 df-count-na
 df-del-index!
 df-del-property!
 df-del-series!
 df-describe
 df-duplicate-series
 df-equal-range
 df-fold
 df-fold/by-index
 df-fold/by-index*
 df-for-each
 df-get-property
 df-has-na?
 df-has-non-na?
 df-index-names
 df-index-of
 df-index-of*
 df-index-series
 df-is-na?
 df-is-sorted?
 df-lookup
 df-lookup*
 df-lookup/interpolated
 df-map
 df-map/by-index
 df-map/by-index*
 df-na-value
 df-property-names
 df-put-property!
 df-ref
 df-ref*
 df-rename-series!
 df-row-count
 df-select
 df-select/by-index
 df-select/by-index*
 df-select*
 df-select*/by-index
 df-select*/by-index*
 df-series-names
 df-set!
 df-set-contract!
 df-set-sorted!
 df-shallow-copy
 foldfn/c
 in-data-frame
 in-data-frame/as-list
 in-data-frame/as-vector
 in-data-frame/by-index
 in-data-frame/by-index*
 in-data-frame/by-index*/as-list
 in-data-frame/by-index*/as-vector
 in-data-frame/by-index/as-list
 in-data-frame/by-index/as-vector
 index/c
 make-data-frame
 mapfn/c
 valid-only

 )

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

