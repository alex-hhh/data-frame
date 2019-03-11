#lang racket/base

;; erg-test.rkt -- tests for data-frame.rkt, ERG files
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

;; NOTE: "raco test -p data-frame" will find this file and run all the tests
;; in it.


(require rackunit
         racket/runtime-path
         "../df.rkt"
         "../erg.rkt")

(define-runtime-path erg-file-1 "./test-data/erg-test-1.erg")
(define-runtime-path erg-file-2 "./test-data/erg-test-2.mrc")
(define-runtime-path erg-file-3 "./test-data/erg-test-3.erg")
(define-runtime-path erg-file-4 "./test-data/erg-test-4.erg")

(define (check-erg-file df)
  (check > (df-row-count df) 0)
  (check-true (df-contains? df "MINUTES") "must contain MINUTES series")
  (check-true (df-contains/any? df "WATTS" "PERCENT") "must contain WATTS or PERCENT series"))

(define erg-tests
  (test-suite
   "ERG tests"
   (test-case "read-erg-files"
     (for ([file (list erg-file-1 erg-file-2 erg-file-3 erg-file-4)])
       (define erg (df-read/erg file))
       (check-erg-file erg)
       ))))

(module+ test
  ;; Tests to be run with raco test
  (require rackunit/text-ui)
  (run-tests erg-tests))
