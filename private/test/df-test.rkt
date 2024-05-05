#lang racket/base

;; df-test.rkt -- tests for data-frame.rkt
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018, 2020, 2021, 2023, 2024 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/math
         racket/list
         racket/base
         racket/match
         racket/port
         racket/vector
         math/statistics
         racket/runtime-path
         db)

(require "../bsearch.rkt"
         "../series.rkt"
         "../exn.rkt"
         "../../spline.rkt"
         "../df.rkt"
         "../sql.rkt"
         "../csv.rkt"
         "../gpx.rkt"
         "../tcx.rkt"
         "../statistics.rkt"
         "../meanmax.rkt"
         "../histogram.rkt"
         "../rdp-simplify.rkt"
         "../least-squares-fit.rkt"
         "../scatter.rkt"
         "../../slr.rkt"
         "../for-df.rkt")

(define-runtime-path csv-test-file "./csv-tests-t1.csv")
(define-runtime-path sample-csv "./test-data/sample.csv")
(define-runtime-path sample2-csv "./test-data/sample2.csv")
(define-runtime-path sample3-csv "./test-data/sample3.csv")
(define-runtime-path sample4-csv "./test-data/sample4.csv")
(define-runtime-path gpx-test-file "./gpx-tests-t1.gpx")
(define-runtime-path sample-gpx-file "./test-data/sample.gpx")
(define-runtime-path sample-tcx-file "./test-data/activity_790564009.tcx")
(define-runtime-path sample-1136-file "./test-data/track-data-1136.csv")
(define-runtime-path lsq-test-file "./test-data/lsq-test.csv")




;;.............................................................. bsearch ....

(define lower+upper-bound-tests
  (test-suite

   "lower-bound/upper-bound"

   (test-case "empty vector"
     (define empty (make-vector 0))
     (check equal? (lower-bound empty 5) 0)
     (check equal? (upper-bound empty 5) 0))

   (test-case "distinct values"
     (define data (for/vector ([x (in-range 1 20)]) x))

     ;; An existing value
     (check equal? (lower-bound data 5) 4)
     (check equal? (upper-bound data 5) 5)

     (let-values ([(low high) (equal-range data 5)])
       (check equal? low 4)
       (check equal? high 5))

     ;; A non-existent value, somewhere inside the vector range
     (check equal? (lower-bound data 4.5) 4)
     (check equal? (upper-bound data 4.5) 4)

     (let-values ([(low high) (equal-range data 4.5)])
       (check equal? low 4)
       (check equal? high 4))

     ;; Non Existent value before the first value in the vector
     (check equal? (lower-bound data -1) 0)
     (check equal? (upper-bound data -1) 0)

     (let-values ([(low high) (equal-range data -1)])
       (check equal? low 0)
       (check equal? high 0))

     ;; Non Existent value after the last value in the vector
     (check equal? (lower-bound data 100) 19)
     (check equal? (upper-bound data 100) 19)
     (let-values ([(low high) (equal-range data 100)])
       (check equal? low 19)
       (check equal? high 19))

     )

   (test-case "reverse order"
     (define data (for/vector ([x (in-range 20 1 -1)]) x))

     ;; An existing value
     (check equal? (lower-bound data 5 #:cmp >) 15)
     (check equal? (upper-bound data 5 #:cmp >) 16)
     (let-values ([(low high) (equal-range data 5 #:cmp >)])
       (check equal? low 15)
       (check equal? high 16))

     ;; A non-existent value, somewhere inside the vector range
     (check equal? (lower-bound data 4.5 #:cmp >) 16)
     (check equal? (upper-bound data 4.5 #:cmp >) 16)
     (let-values ([(low high) (equal-range data 4.5 #:cmp >)])
       (check equal? low 16)
       (check equal? high 16))

     ;; Non Existent value before the first value in the vector
     (check equal? (lower-bound data -1 #:cmp >) 19)
     (check equal? (upper-bound data -1 #:cmp >) 19)
     (let-values ([(low high) (equal-range data -1 #:cmp >)])
       (check equal? low 19)
       (check equal? high 19))

     ;; Non Existent value after the last value in the vector
     (check equal? (lower-bound data 100 #:cmp >) 0)
     (check equal? (upper-bound data 100 #:cmp >) 0)
     (let-values ([(low high) (equal-range data 100 #:cmp >)])
       (check equal? low 0)
       (check equal? high 0)))

   (test-case "duplicate values"
     (define data (vector 1 2 3 3 3 4 5 6))

     ;; An existing value
     (check equal? (lower-bound data 3) 2)
     (check equal? (upper-bound data 3) 5)
     (let-values ([(low high) (equal-range data 3)])
       (check equal? low 2)
       (check equal? high 5)))

   (test-case "other"
     (define data (for/vector ([x (in-range 1 21)]) x)) ; 20 element vector

     ;; Sub-range searching, basics
     (check equal? (lower-bound data 5 #:start 0 #:stop 3) 3)
     (check equal? (lower-bound data 5 #:start 3 #:stop 7) 4)
     (check equal? (lower-bound data 5 #:stop 3) 3)
     (check equal? (lower-bound data 5 #:start 7) 7)

     (check equal? (upper-bound data 5 #:start 0 #:stop 3) 3)
     (check equal? (upper-bound data 5 #:start 3 #:stop 7) 5)
     (check equal? (upper-bound data 5 #:stop 3) 3)
     (check equal? (upper-bound data 5 #:start 7) 7)

     ;; Searches in ranges of 1 and 0 length ranges
     (check equal? (lower-bound data 5 #:start 4 #:stop 5) 4)
     (check equal? (lower-bound data 5 #:start 3 #:stop 3) 3)

     (check equal? (upper-bound data 5 #:start 4 #:stop 5) 5)
     (check equal? (upper-bound data 5 #:start 3 #:stop 3) 3)

     ;; ;; Off the grid searches
     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is out of range, should raise exception
        (lower-bound data 5 #:start -100 #:stop 5)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is out of range, should raise exception
        (upper-bound data 5 #:start -100 #:stop 5)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; end is out of range, should raise exception
        (lower-bound data 5 #:start 0 #:stop 200)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; end is out of range, should raise exception
        (upper-bound data 5 #:start 0 #:stop 200)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is after end
        (lower-bound data 5 #:start 5 #:stop 1)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; start is after end
        (upper-bound data 5 #:start 5 #:stop 1))))))


;;................................................................ series ....

(define series-tests
  (test-suite
   "df-series"

   (test-case "df-series: wrong sort order"
     (check-exn
      exn:fail:data-frame?
      (lambda () (make-series "col1" #:data #(1 2 3) #:cmpfn >))))

   (test-case "df-series: almost equal values"
     (check-not-exn
      (lambda ()
        ;; (equal? 0 0.0) is #f, but we want to make sure a series which
        ;; contains such values is still considered sorted...
        (make-series "c" #:data #(0 0.0 0) #:cmpfn <))))

   (test-case "df-series: wrong contract"
     (check-exn
      exn:fail:data-frame?
      (lambda () (make-series "col1" #:data #(1 2 3) #:contract string?))))

   (test-case "df-series: other"

     (define c1 (make-series "col1" #:capacity 10))
     (check = (series-size c1) 0)
     (check = (series-free-space c1) 10)
     (check-true (series-empty? c1))
     (series-reserve-space c1 100)
     (check = (series-free-space c1) 100)
     (check-false (series-is-sorted? c1))

     ;; NOTE: c1 is empty
     (check-false (series-has-non-na? c1))
     (check-false (series-has-na? c1))

     (define c2 (make-series "col2" #:data #(1 2 3) #:contract integer? #:cmpfn <))
     (check = (series-size c2) 3)
     (check = (series-free-space c2) 0)
     (check = (series-ref c2 1) 2)
     (check-exn
      exn:fail:contract?
      (lambda ()
        (series-ref c2 10)))
     (series-push-back! c2 5)
     (check = (series-size c2) 4)
     (check = (series-ref c2 3) 5)
     (check-exn
      exn:fail:contract?
      ;; cannot add a non sorted element
      (lambda () (series-push-back! c2 1)))

     (check-false (series-has-na? c2))
     (check-true (series-has-non-na? c2))
     (check-true (series-is-sorted? c2))

     (check equal? (for/list ((x (in-series c1))) x) '())
     (check equal? (for/list ((x (in-series c2))) x) '(1 2 3 5))

     (check equal? (series-index-of c2 3) 2)
     (let-values ([(low upr) (series-equal-range c2 1)])
       (check equal? low 0)
       (check equal? upr 1))
     (let-values ([(low upr) (series-equal-range c2 3)])
       (check equal? low 2)
       (check equal? upr 3))

     (check-not-exn
      (lambda ()
        (series-set-sorted! c2 <)))

     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (series-set-contract! c2 string?)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 contains only integers
        (series-push-back! c2 "abc")))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 contains only integers
        (series-push-back! c2 6.5)))

     (check-not-exn
      (lambda ()
        (series-set-contract! c2 real?)))

     (check-not-exn
      (lambda ()
        ;; c2 now contains reals
        (series-push-back! c2 6.5)))

     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (series-set-sorted! c2 >)))

     (check equal? (series-na-count c2) 0)    ; no NA values in C2
     (check equal? (series-index-of c2 3) 2)

     (check-exn
      exn:fail:contract?
      (lambda ()
        (series-push-back! c2 4)))
     (check = (series-size c2) 5)
     (check-not-exn
      (lambda ()
        (series-push-back! c2 7)))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; c2 is marked sorted, as such we cannot set a NA value
        (series-set! c2 3 #f)))

     (check-not-exn
      (lambda ()
        ;; remove sort constrain on c2
        (series-set-sorted! c2 #f)))

     (check-not-exn
      (lambda ()
        (series-set! c2 3 #f)))

     (check equal? (series-ref c2 3) #f)
     (check equal? (series-na-count c2) 1) ; C2 has one NA value

     (define c3 (make-series "c3" #:data #(1 1 1 2 2 2 2 3 3 3) #:cmpfn <))

     (let-values ([(low upr) (series-equal-range c3 1)])
       (check equal? low 0)
       (check equal? upr 3))
     (let-values ([(low upr) (series-equal-range c3 2)])
       (check equal? low 3)
       (check equal? upr 7))
     (let-values ([(low upr) (series-equal-range c3 3)])
       (check equal? low 7)
       (check equal? upr 10))

     (let-values ([(low upr) (series-equal-range c3 1.5)])
       (check equal? low 3)
       (check equal? upr 3))
     (let-values ([(low upr) (series-equal-range c3 2.3)])
       (check equal? low 7)
       (check equal? upr 7))
     (let-values ([(low upr) (series-equal-range c3 2.7)])
       (check equal? low 7)
       (check equal? upr 7))

     (define c4 (copy-series c2))
     (check-false (equal? c2 c4))
     (check-true (equal? (series-name c2) (series-name c4)))
     (check-true (for/and ([a (in-series c2)] [b (in-series c4)])
                   (equal? a b)))
     (check-false (series-is-sorted? c4))

     (check-true (series-is-sorted? (copy-series c3))))))


;;...................................................... secondary-index ....

(define sx-tests
  (test-suite
   "sx"

   (test-case "make-sx-comparator"
     ;; NA's are greater than everything else (they are at the back)
     (let ([cmp (make-sx-comparator (list string<? <) '(#f #f) #f)])

       ;; NOTE: the first element in the keys is a position and does not
       ;; participate in the comparison.
       (check-true (cmp '(12 "Alpha" 1) '(13 "Omega" 1)))
       (check-false (cmp '(14 "Omega" 1) '(15 "Alpha" 1)))

       (check-true (cmp '(16 "Hello" 1) '(17 "Hello" 2)))
       (check-false (cmp '(18 "Hello" 2) '(19 "Hello" 1)))
       (check-false (cmp '(20 "Hello" 1) '(21 "Hello" 1)))

       ;; Incomplete keys are equal to a more complete key -- note that both
       ;; "less than" directions are false
       (check-false (cmp '(22 "Hello") '(23 "Hello" 1)))
       (check-false (cmp '(24 "Hello" 1) '(25 "Hello")))

       ;; NA's at the back
       (check-true (cmp '(26 "Hello" 1) '(27 "Hello" #f)))
       (check-false (cmp '(27 "Hello" #f) '(26 "Hello" 1)))
       (check-true (cmp '(29 "Hello" 2) '(28 #f 1)))
       (check-false (cmp '(28 #f 1) '(29 "Hello" 2)))

       (check-true (cmp '(30 #f 1) '(31 #f 2)))
       (check-false (cmp '(31 #f 2) '(30 #f 1))))

     ;; NA's are smaller than everything else (they are in front)
     (let ([cmp (make-sx-comparator (list string<? <) '(#f #f) #t)])

       ;; NOTE: the first element in the keys is a position and does not
       ;; participate in the comparison.
       (check-true (cmp '(12 "Alpha" 1) '(13 "Omega" 1)))
       (check-false (cmp '(14 "Omega" 1) '(15 "Alpha" 1)))

       (check-true (cmp '(16 "Hello" 1) '(17 "Hello" 2)))
       (check-false (cmp '(18 "Hello" 2) '(19 "Hello" 1)))
       (check-false (cmp '(20 "Hello" 1) '(21 "Hello" 1)))

       ;; Incomplete keys are equal to a more complete key -- note that both
       ;; "less than" directions are false
       (check-false (cmp '(22 "Hello") '(23 "Hello" 1)))
       (check-false (cmp '(24 "Hello" 1) '(25 "Hello")))

       ;; NA's at the front
       (check-false (cmp '(26 "Hello" 1) '(27 "Hello" #f)))
       (check-true (cmp '(27 "Hello" #f) '(26 "Hello" 1)))
       (check-false (cmp '(29 "Hello" 2) '(28 #f 1)))
       (check-true (cmp '(28 #f 1) '(29 "Hello" 2)))

       (check-true (cmp '(30 #f 1) '(31 #f 2)))
       (check-false (cmp '(31 #f 2) '(30 #f 1))))

     ;; This shows a comparator where the NA value for the second column is 3.
     (let ([cmp/na-front (make-sx-comparator (list string<? <) '(#f 3) #t)]
           [cmp/na-back (make-sx-comparator (list string<? <) '(#f 3) #f)])

       ;; OK, so 1 is less than 3, we know that :-)
       (check-true (cmp/na-back '(12 "Alpha" 1) '(13 "Alpha" 3)))
       (check-false (cmp/na-back '(14 "Alpha" 3) '(15 "Alpha" 1)))

       ;; ... since 3 is a NA value, 4 is also less than 3...
       (check-true (cmp/na-back '(12 "Alpha" 4) '(13 "Alpha" 3)))
       (check-false (cmp/na-back '(15 "Alpha" 3) '(14 "Alpha" 4)))

       ;; ... but if NAs are in front, 3 is less than 1
       (check-false (cmp/na-front '(12 "Alpha" 1) '(13 "Alpha" 3)))
       (check-true (cmp/na-front '(14 "Alpha" 3) '(15 "Alpha" 1))))

     )))


;;............................................................... spline ....

(define spline-tests
  (test-suite
   "spline"

   (test-case "spline: other"
     (define data-points '(#(-1 0.5) #(0 0) #(3 3)))
     (define fn (spline data-points))
     (check equal? (fn 0) 0)
     (check-pred real? (fn 0.5))
     (check-pred real? (fn 1.0))
     (check-pred real? (fn -2))         ; outside the points range
     (check-pred real? (fn 10)))))         ; outside the points range

     ;;
     ;; This will plot the spline and the know points, useful for
     ;; visualization
     ;;
     ;; (require plot)
     ;; (send (plot-frame (list (function fn)
     ;;                    (points data-points))
     ;;              #:x-min -5 #:x-max 5) show #t)




;;........................................................... data-frame ....

;; These functions are not tested...

;;  (df-put-property (-> data-frame? symbol? any/c any/c))
;;  (df-get-property (->* (data-frame? symbol?) ((-> any/c)) any/c))
;;  (df-del-property (-> data-frame? symbol? any/c))
;;  (df-set-default-weight-series (-> data-frame? string? any/c))
;;  (df-get-default-weight-series (-> data-frame? (or/c #f string?)))
;;  (valid-only (-> any/c boolean?))

(define df-tests
  (test-suite
   "df"

   (test-case "df: other"

     ;; Basic construction and item access
     (define df (make-data-frame))
     ;; This data frame is empty
     (check equal? (df-series-names df) '())
     (check equal? (df-property-names df) '())
     (check equal? (df-row-count df) 0)

     (define c1 (make-series "col1" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))
     (df-add-series! df c1)
     (define c2 (make-series "col2" #:data #(3 2 1 0) #:contract integer? #:cmpfn >))
     (df-add-series! df c2)

     (define c6 (make-series "col6" #:data #(1 2 3 4) #:contract integer?))
     (df-add-series! df c6)
     (check-false (df-is-sorted? df "col6"))
     (check-not-exn
      (lambda ()
        (df-set-sorted! df "col6" <)))
     (check-true (df-is-sorted? df "col6"))
     (check-exn
      exn:fail:data-frame?
      (lambda ()
        ;; wrong sort order
        (df-set-sorted! df "col6" >)))
     ;; attempting sorting with wrong sort order didn't destroy the previous sorting
     (check-true (df-is-sorted? df "col6"))

     (define c7 (make-series "col7"
                             #:data #(1 1 2 2)
                             #:contract integer? #:cmpfn <))
     (df-add-series! df c7)

     (check-true (df-is-sorted? df "col7"))
     (check equal? (df-index-of df "col7" 1) 0)
     (let-values ([(low upr) (df-equal-range df "col7" 1)])
       (check equal? low 0)
       (check equal? upr 2))
     (check equal? (df-index-of df "col7" 2) 2)
     (let-values ([(low upr) (df-equal-range df "col7" 2)])
       (check equal? low 2)
       (check equal? upr 4))

     (check-not-exn
      (lambda ()
        ;; note: this probably needs more tests...
        (df-shallow-copy df))))

   (test-case "df-add-series!"
     (define df (make-data-frame))
     (define c0 (make-series "c0" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))
     (check-not-exn (lambda () (df-add-series! df c0)))
     (check-true (df-contains? df "c0"))
     (check equal? (df-count-na df "c0") 0)
     (check-true (df-is-sorted? df "c0"))
     (check = (df-row-count df) 4)

     ;; Add another series with the same number of elements.
     (define c1 (make-series "c1" #:data #(3 2 1 0) #:contract integer? #:cmpfn >))
     (check-not-exn (lambda () (df-add-series! df c1)))

     (check-true (df-contains? df "c0" "c1"))
     ;; NOTE: all series must be present, c-non-existent is not
     (check-false (df-contains? df "c0" "c-non-existent"))
     ;; Any series must be present
     (check-true (df-contains/any? df "c0" "c-non-existent"))

     ;; This will fail as col0 has 2 rows in a data frame that has 4, and this
     ;; should fail.
     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (df-add-series! df (make-series "c2" #:data #(1 2)))))

     ;; A series can be replaced by df-add-series!
     (check-equal? (df-select df "c1") #(3 2 1 0))
     (df-add-series! df (make-series "c1" #:data #(5 6 7 8)))
     (check-equal? (df-select df "c1") #(5 6 7 8))

     ;; A series can be deleted.
     (check-not-exn (lambda () (df-del-series! df "c1")))
     (check-false (df-contains? df "c1"))
     ;; Deleting a non-existent series should be OK
     (check-not-exn (lambda () (df-del-series! df "c1")))

     ;; Check that an index is correctly rebuilt when an indexed series is
     ;; rebuilt
     (df-add-series! df (make-series "c" #:data #(3 2 1 0)))
     (df-add-index! df "i" "c" <)
     (check-equal? (df-select/by-index df #:index "i" "c") #(0 1 2 3))
     ;; Replace "c"
     (df-add-series! df (make-series "c" #:data #(4 3 2 1)))
     (check-equal? (df-select/by-index df #:index "i" "c") #(1 2 3 4)))

   (test-case "df-add-derived!"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (check-not-exn
      (lambda ()
        (df-add-derived!
         df "c2" '("c0" "c1")
         (lambda (v)
           (match-define (list c1 c2) v)
           (+ c1 c2)))))
     (check-equal? (df-select df "c2") #(4 4 4 4))

     ;; Check that an index is correctly rebuilt when an indexed series is
     ;; rebuilt
     (df-add-series! df (make-series "c" #:data #(3 2 1 0)))
     (df-add-index! df "i" "c" <)
     (check-equal? (df-select/by-index df #:index "i" "c") #(0 1 2 3))
     ;; Rebuilt "c"
     (df-add-derived! df "c" '("c") (lambda (v) (add1 (list-ref v 0))))
     (check-equal? (df-select/by-index df #:index "i" "c") #(1 2 3 4)))

   (test-case "df-add-lazy!"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-series! df (make-series "c2" #:data #(0 1 2 3)))

     (check-not-exn
      (lambda ()
        (df-add-lazy!
         df "c3" '("c1" "c2")
         (lambda (v)
           (match-define (list c1 c2) v)
           (+ c1 c2)))))
     (check-true (df-contains? df "c3"))
     ;; c3 will be materialized now...
     (check equal? (df-select df "c3") #(3 3 3 3))

     ;; Attempt to lazily replace an existing data series should fail...
     (check-exn
      exn:fail:data-frame?
      (lambda ()
        (df-add-lazy! df "c1" '("c3" "c1") (lambda (v) (add1 (list-ref v 0)))))))

   (test-case "df-rename-series!"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c1" #:data #(0 1 2 3)))
     (df-add-lazy! df "c2" '("c1") (lambda (v) (list-ref v 0)))
     (df-add-index! df "i1" "c1" <)

     (df-rename-series! df "c1" "c0")
     (check-true (df-contains? df "c0"))
     (check-true (df-contains? df "c2"))
     (check-false (df-contains? df "c1"))
     (check-equal? (df-index-of df "c0" 2) 2))

   (test-case "df-set!, df-ref, df-ref*"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))

     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; only integer? values can be set -- there is a #:contract on this
        ;; series
        (df-set! df 0 1.5 "c0")))
     (check-exn
      exn:fail:contract?
      (lambda ()
        ;; Setting the value 100 will break sort order on the column
        (df-set! df 1 100 "c0")))
     (check-not-exn
      (lambda ()
        ;; This is OK, as it matches the contract and does not break sort
        ;; order
        (df-set! df 0 -1 "c0")))
     ;; Check that it was indeed set!
     (check = (df-ref df 0 "c0") -1)

     (check = (df-ref df 1 "c0") 2)
     (df-add-series! df (make-series "c1" #:data #(2 3 2 2)))
     (check equal? (df-ref* df 1 "c0" "c1") #(2 3))

     ;; Check that an index is correctly rebuilt when a value in an indexed
     ;; series changes
     (df-add-series! df (make-series "c" #:data #(3 2 1 0)))
     (df-add-index! df "i" "c" <)
     (check-equal? (df-select/by-index df #:index "i" "c") #(0 1 2 3))
     ;; Update one value in "c"
     (df-set! df 1 4 "c")
     (check = (df-ref df 1 "c") 4)
     (check-equal? (df-select/by-index df #:index "i" "c") #(0 1 3 4)))

   (test-case "df-index-of, df-index-of*, df-equal-range"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))

     (check = (df-index-of df "c0" 2) 1)
     (check = (df-index-of df "c0" 1.5) 1) ; non existent value, find best match
     (check equal? (df-index-of df "c0" 1.5 #:exact-match? #t) #f) ; not found
     (check = (df-index-of df "c0" -1) 0)
     (check equal? (df-index-of df "c0" -1 #:exact-match? #t) #f)
     (check = (df-index-of df "c0" 100) 4)
     (check equal? (df-index-of df "c0" 100 #:exact-match? #t) #f)

     (check equal? (df-index-of* df "c0" 2 -1 100) '(1 0 4))
     (check equal? (df-index-of* df "c0" 2 -1 100 #:exact-match? #t) '(1 #f #f))

     (let-values ([(low upr) (df-equal-range df "c0" 2)])
       (check equal? low 1)
       (check equal? upr 2))
     (let-values ([(low upr) (df-equal-range df "c0" 1.5)])
       (check equal? low 1)
       (check equal? upr 1))
     (let-values ([(low upr) (df-equal-range df "c0" -1)])
       (check equal? low 0)
       (check equal? upr 0))
     (let-values ([(low upr) (df-equal-range df "c0" 100)])
       (check equal? low 4)
       (check equal? upr 4))

     ;; lookup using indexes
     (df-add-series! df (make-series "c1" #:data #(4 3 2 1)))
     (df-add-index! df "i1" "c1" <)

     (check = (df-index-of df "c1" 2) 2)
     (check = (df-index-of df "c1" 1.5) 2) ; non existent value, find best match
     (check equal? (df-index-of df "c1" 1.5 #:exact-match? #t) #f) ; not found
     (check = (df-index-of df "c1" -1) 3)
     (check equal? (df-index-of df "c1" -1 #:exact-match? #t) #f)
     (check = (df-index-of df "c1" 100) 4)
     (check equal? (df-index-of df "c1" 100 #:exact-match? #t) #f)

     (check equal? (df-index-of* df "c1" 2 -1 100) '(2 3 4))
     (check equal? (df-index-of* df "c1" 2 -1 100 #:exact-match? #t) '(2 #f #f)))

   (test-case "df-all-indices-of"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4 2 5 2 6)))
     (df-add-index! df "i0" "c0" <)
     ;; Return all the positions where 2 is located
     (check equal? (df-all-indices-of df "c0" 2) '(1 4 6)))

   (test-case "df-lookup, df-lookup*"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))

     (check equal? (df-lookup df "c0" "c1" 3) 1)
     (check equal? (df-lookup df "c0" '("c0" "c1") 3) #(3 1))
     (check equal? (df-lookup df "c0" "c1" -1) 3)
     (check equal? (df-lookup df "c0" "c1" 100) #f)
     (check equal? (df-lookup df "c0" "c1" 1.5) 2)
     (check equal? (df-lookup df "c0" "c1" 1.5 #:exact-match? #t) #f)

     (check equal? (df-lookup* df "c0" "c1" 1 4) '(3 0))
     (check equal? (df-lookup* df "c0" "c1" -1 1 2.5 4 100) '(3 3 1 0 #f))
     (check equal? (df-lookup* df "c0" "c1" -1 1 2.5 4 100 #:exact-match? #t) '(#f 3 #f 0 #f))
     (check equal? (df-lookup* df "c0" '("c0" "c1") 1 4) '(#(1 3) #(4 0)))

     ;; Lookup via an multi-column index
     (df-add-index*! df "i1" '("c1" "c0") (list < <))

     (check equal? (df-lookup df "c1" "c0" 1) 3)
     (check equal? (df-lookup df "c1" "c0" 1.5) 2)
     (check equal? (df-lookup df "c1" "c0" 1.5 #:exact-match? #t) #f)

     (check equal? (df-lookup* df "c1" "c0" 1 1.5 3) '(3 2 1))
     (check equal? (df-lookup* df "c1" "c0" 1 1.5 3 #:exact-match? #t) '(3 #f 1)))

   (test-case "df-lookup/interpolated"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4) #:contract integer? #:cmpfn <))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0) #:contract integer? #:cmpfn >))

     (check = (df-lookup/interpolated df "c0" "c1" 0) 3) ; out of range
     (check = (df-lookup/interpolated df "c0" "c1" 1) 3) ; at the beginning
     (check = (df-lookup/interpolated df "c0" "c1" 4) 0) ; at the end
     (check = (df-lookup/interpolated df "c0" "c1" 5) 0) ; out of range
     (check < (abs (- (df-lookup/interpolated df "c0" "c1" 2.2) 1.8)) 0.001)

     (check equal? (df-lookup/interpolated df "c0" '("c0" "c1") 0) #(1 3)) ; out of range
     (check equal? (df-lookup/interpolated df "c0" '("c0" "c1") 1) #(1 3)) ; at the beginning
     (check equal? (df-lookup/interpolated df "c0" '("c0" "c1") 4) #(4 0)) ; at the end
     (check equal? (df-lookup/interpolated df "c0" '("c0" "c1") 5) #(4 0)) ; out of range
     (let ((val (df-lookup/interpolated df "c0" '("c0" "c1") 2.2)))
       (check < (abs (- (vector-ref val 0) 2.2)) 0.001)
       (check < (abs (- (vector-ref val 1) 1.8)) 0.001)))

   (test-case "df-select, df-select*"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))

     (check equal? (df-select df "c0") #(1 2 3 4))
     (check equal? (df-select df "c0" #:filter odd?) #(1 3))
     (check equal? (df-select df "c0" #:start 1 #:stop 3) #(2 3))
     (check equal? (df-select* df "c0" "c1") #(#(1 3) #(2 2) #(3 1) #(4 0)))
     (check equal? (df-select* df "c0" "c1" #:start 1 #:stop 3) #(#(2 2) #(3 1)))
     (define result (df-select* df "c0" "c1" #:start 1 #:stop 2
                                #:filter (lambda (val)
                                           (check-pred vector? val)
                                           (check = (vector-length val) 2)
                                           (match-define (vector c1 c2) val)
                                           (check = c1 2)
                                           (check = c2 2)
                                           #f)))
     (check-true (and (vector? result) (= (vector-length result) 0))))

   (test-case "df-select/by-index variants"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-series! df (make-series "c2" #:data #("Omega" "Alpha" "Alpha" "Omega")))
     (df-add-index*! df "i1" '("c2" "c1") (list string<? <))

     (check equal? (df-select/by-index df "c0" #:index "i1") #(3 2 4 1))
     (check equal? (df-select/by-index df "c0" #:index "i1" #:from "Omega" #:to "Omega")
            #(4 1))
     (check equal? (df-select*/by-index df "c0" "c1" #:index "i1")
            #(#(3 1) #(2 2) #(4 0) #(1 3)))
     (check equal? (df-select*/by-index df "c0" "c1" #:index "i1" #:from "Alpha" #:to "Alpha")
            #(#(3 1) #(2 2)))

     (check equal? (df-select/by-index* df "c0" #:index "i1") #(3 2 4 1))
     (check equal? (df-select/by-index* df "c0" #:index "i1"
                                        #:from '("Alpha" 2) #:to '("Omega" 1))
            #(2 4))
     (check equal? (df-select*/by-index* df "c0" "c1" #:index "i1")
            #(#(3 1) #(2 2) #(4 0) #(1 3)))
     (check equal? (df-select*/by-index* df "c0" "c1" #:index "i1"
                                         #:from '("Alpha" 2) #:to '("Omega" 1))
            #(#(2 2) #(4 0))))

   (test-case "df-map variants"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-series! df (make-series "c2" #:data #(1 2 3 4)))
     (df-add-index*! df "i1" '("c1" "c2") (list < <))

     (check equal?
            (df-map df
                    '("c0" "c1")
                    (lambda (prev current)
                      (if prev
                          ;; A delta series on col1, col2
                          (match-let (((list pc1 pc2) prev)
                                      ((list c1 c2) current))
                            (list (- c1 pc1) (- c2 pc2)))
                          'none)))
            #(none (1 -1) (1 -1) (1 -1)))

     (check equal?
            (df-map df
                    '("c0" "c1")
                    (lambda (current)
                      (match-let (((list c1 c2) current))
                        (+ c1 c2))))
            #(4 4 4 4))

     (check equal?
            (df-map df
                    '("c0" "c1")
                    (lambda (prev current)
                      (if prev
                          ;; A delta series on col1, col2
                          (match-let (((list pc1 pc2) prev)
                                      ((list c1 c2) current))
                            (list (- c1 pc1) (- c2 pc2)))
                          'none))
                    #:start 1 #:stop 3)
            #(none (1 -1)))

     (check equal?
            (df-map/by-index
             df
             '("c0" "c2")
             #:index "i1"
             (lambda (prev current)
               (if prev
                   ;; A delta series on col1, col2
                   (match-let (((list pc1 pc2) prev)
                               ((list c1 c2) current))
                     (list (- c1 pc1) (- c2 pc2)))
                   'none)))
            #(none (-1 -1) (-1 -1) (-1 -1)))
     (check equal?
            (df-map/by-index
             df
             '("c0" "c2")
             #:index "i1"
             (lambda (current)
               (match-let (((list c1 c2) current))
                 (- c1 c2))))
             #(0 0 0 0))
     (check equal?
            (df-map/by-index
             df
             '("c0" "c2")
             #:index "i1" #:from 1 #:to 2
             (lambda (current)
               (match-let (((list c1 c2) current))
                 (+ c1 c2))))
            #(6 4))
     (check equal?
            (df-map/by-index*
             df
             '("c0" "c2")
             #:index "i1"
             (lambda (current)
               (match-let (((list c1 c2) current))
                 (- c1 c2))))
            #(0 0 0 0))
     (check equal?
            (df-map/by-index*
             df
             '("c0" "c2")
             #:index "i1" #:from '(1 3) #:to '(2 2)
             (lambda (current)
               (match-let (((list c1 c2) current))
                 (+ c1 c2))))
            #(6 4)))

   (test-case "df-for-each variants"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-series! df (make-series "c2" #:data #(1 2 3 4)))
     (df-add-index*! df "i1" '("c1" "c2") (list < <))

     (define result1 '())
     (df-for-each df
                  '("c0" "c1")
                  (lambda (prev current)
                    (define v (if prev
                                  ;; A delta series on col1, col2
                                  (match-let (((list pc1 pc2) prev)
                                              ((list c1 c2) current))
                                    (list (- c1 pc1) (- c2 pc2)))
                                  'none))
                    (set! result1 (cons v result1)))
                  #:start 1 #:stop 3)
     (check equal? result1 '((1 -1) none))

     (define result2 '())
     (df-for-each df
                  '("c0" "c1")
                  (lambda (prev current)
                    (define v (if prev
                                  ;; A delta series on col1, col2
                                  (match-let (((list pc1 pc2) prev)
                                              ((list c1 c2) current))
                                    (list (- c1 pc1) (- c2 pc2)))
                                  'none))
                    (set! result2 (cons v result2))))
     (check equal? result2 '((1 -1) (1 -1) (1 -1) none))

     (define result3 '())
     (df-for-each df
                  '("c0" "c1")
                  (lambda (current)
                    (define v (match-let (((list c1 c2) current))
                                (+ c1 c2)))
                    (set! result3 (cons v result3))))
     (check equal? result3 '(4 4 4 4))

     (define result4 '())
     (df-for-each/by-index
      df
      '("c0" "c2")
      #:index "i1"
      (lambda (current)
        (define v (match-let (((list c1 c2) current))
                    (+ c1 c2)))
        (set! result4 (cons v result4))))
     (check equal? result4 '(2 4 6 8))  ; note that result4 is in reverse

     (define result5 '())
     (df-for-each/by-index
      df
      '("c0" "c2")
      #:index "i1" #:from 2 #:to 3
      (lambda (current)
        (define v (match-let (((list c1 c2) current))
                    (+ c1 c2)))
        (set! result5 (cons v result5))))
     (check equal? result5 '(2 4))      ; note that result5 is in reverse

     (define result6 '())
     (df-for-each/by-index*
      df
      '("c0" "c2")
      #:index "i1" #:from '(2) #:to '(3)
      (lambda (current)
        (define v (match-let (((list c1 c2) current))
                    (+ c1 c2)))
        (set! result6 (cons v result6))))
     (check equal? result6 '(2 4))      ; note that result6 is in reverse

     )

   (test-case "df-fold variants"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-series! df (make-series "c2" #:data #(1 2 3 4)))
     (df-add-index*! df "i1" '("c1" "c2") (list < <))

     (check equal? (df-fold df
                            '("c0" "c1") ; series
                            0            ; initial value
                            (lambda (accumulator v)
                              (match-define (list c1 c2) v)
                              (+ accumulator c1 c2)))
            16)
     (check equal? (df-fold df
                            '("c0" "c1") ; series
                            0            ; initial value
                            (lambda (accumulator prev current)
                              (if prev
                                  (match-let (((list c1 c2) prev)
                                              ((list c3 c4) current))
                                    (+ accumulator c1 c2 c3 c4))
                                  accumulator)))
            24)
     (check equal? (df-fold df
                            '("c0" "c1") ; series
                            0                ; initial value
                            (lambda (accumulator v)
                              (match-define (list c1 c2) v)
                              (+ accumulator c1 c2))
                            #:start 1 #:stop 4)
            12)
     (check equal? (df-fold df
                            '("c0" "c1") ; series
                            0                ; initial value
                            (lambda (accumulator prev current)
                              (if prev
                                  (match-let (((list c1 c2) prev)
                                              ((list c3 c4) current))
                                    (+ accumulator c1 c2 c3 c4))
                                  accumulator))
                            #:start 1 #:stop 4)
            16)
     (check equal? (df-fold/by-index
                    df
                    '("c0" "c2") ; series
                    0                ; initial value
                    (lambda (accumulator current)
                      (match-let (((list c0 c1) current))
                        (+ accumulator c0 c1)))
                    #:index "i1" #:from 1 #:to 2)
            10)
     (check equal? (df-fold/by-index*
                    df
                    '("c0" "c2") ; series
                    0                ; initial value
                    (lambda (accumulator current)
                      (match-let (((list c0 c1) current))
                        (+ accumulator c0 c1)))
                    #:index "i1" #:from '(1) #:to '(2))
            10))

   (test-case "in-data-frame variants"
     (define df (make-data-frame))
     (df-add-series! df (make-series "c0" #:data #(1 2 3 4)))
     (df-add-series! df (make-series "c1" #:data #(3 2 1 0)))
     (df-add-index! df "i1" "c1" <)

     (check-equal?
      (for/list ([(c0 c1) (in-data-frame df "c0" "c1" #:start 1 #:stop 3)])
        (cons c0 c1))
      '((2 . 2) (3 . 1)))
     (check-equal?
      (for/list ([(c0 c1) (in-data-frame df "c0" "c1")])
        (cons c0 c1))
      '((1 . 3) (2 . 2) (3 . 1) (4 . 0)))
     ;; See if we can select backwards
     (check-equal?
      (for/list ([(c0 c1) (in-data-frame df "c0" "c1" #:start 2 #:stop -1)])
        (cons c0 c1))
      '((3 . 1) (2 . 2) (1 . 3)))

     (check-equal?
      (for/list ([item (in-data-frame/as-list df "c0" "c1" #:start 1 #:stop 3)])
        item)
      '((2 2) (3 1)))
     (check-equal?
      (for/list ([item (in-data-frame/as-list df "c0" "c1")])
        item)
      '((1 3) (2 2) (3 1) (4 0)))
     ;; See if we can select backwards
     (check-equal?
      (for/list ([item (in-data-frame/as-list df "c0" "c1" #:start 2 #:stop -1)])
        item)
      '((3 1) (2 2) (1 3)))

     (check-equal?
      (for/list ([item (in-data-frame/as-vector df "c0" "c1" #:start 1 #:stop 3)])
        item)
      '(#(2 2) #(3 1)))
     (check-equal?
      (for/list ([item (in-data-frame/as-vector df "c0" "c1")])
        item)
      '(#(1 3) #(2 2) #(3 1) #(4 0)))
     ;; See if we can select backwards
     (check-equal?
      (for/list ([item (in-data-frame/as-vector df "c0" "c1" #:start 2 #:stop -1)])
        item)
      '(#(3 1) #(2 2) #(1 3)))

     (check-equal?
      (for/list ([(c0 c1) (in-data-frame/by-index df "c0" "c1" #:index "i1")])
        (cons c0 c1))
      '((4 . 0) (3 . 1) (2 . 2) (1 . 3)))
     (check-equal?
      (for/list ([item (in-data-frame/by-index/as-list df "c0" "c1" #:index "i1")])
        item)
      '((4 0) (3 1) (2 2) (1 3)))
     (check-equal?
      (for/list ([item (in-data-frame/by-index/as-vector df "c0" "c1" #:index "i1")])
        item)
      '(#(4 0) #(3 1) #(2 2) #(1 3)))

     (check-equal?
      (for/list ([(c0 c1) (in-data-frame/by-index* df "c0" "c1" #:index "i1")])
        (cons c0 c1))
      '((4 . 0) (3 . 1) (2 . 2) (1 . 3)))
     (check-equal?
      (for/list ([item (in-data-frame/by-index*/as-list df "c0" "c1" #:index "i1")])
        item)
      '((4 0) (3 1) (2 2) (1 3)))
     (check-equal?
      (for/list ([item (in-data-frame/by-index*/as-vector df "c0" "c1" #:index "i1")])
        item)
      '(#(4 0) #(3 1) #(2 2) #(1 3))))
   ))

(define (with-fresh-database thunk)
  (let ((db (sqlite3-connect #:database 'memory #:mode 'create)))
    (dynamic-wind
      (lambda () (void))
      ;; NOTE: cannot really catch errors as error trace will loose context
      (lambda () (thunk db))
      (lambda () (disconnect db)))))

(define dfdb-tests
  (test-suite
   "df-read/sql"

   (test-case "df-read/sql: other"
     (with-fresh-database
       (lambda (db)
         (query-exec db "create table T(a real, b real, c text)")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 1 2 "alpha")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 2 4 "beta")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 4 8 "gamma")
         (query-exec db "insert into T(a, b, c) values (?, ?, ?)" 8 16 "delta")

         (define df (df-read/sql db "select a, b, c from T order by a"))
         (check-true (df-contains? df "a" "b" "c"))
         (check equal? (df-select df "a") #(1.0 2.0 4.0 8.0))
         (check equal? (df-select df "b") #(2.0 4.0 8.0 16.0))
         (check equal? (df-select df "c") #("alpha" "beta" "gamma" "delta")))))))

(define csv-tests
  (test-suite
   "df-read+write/csv"
   #:after (lambda ()
             (with-handlers (((lambda (e) #t) (lambda (e) #t)))
               (delete-file csv-test-file)))

   (test-case "df-write/csv: empty dataframe"
     (define df (make-data-frame))
     ;; Check that df-write/csv does not go into an infinite loop when writing
     ;; out an empty data frame (it used to :-) )
     (define t (thread (lambda ()
                         (call-with-output-string
                          (lambda (out) (df-write/csv df out))))))
     (check-true (not (eq? #f (sync/timeout 10.0 t)))
                 "infinite loop in df-write/csv")
     (kill-thread t))

   (test-case "df-read/csv: basic"
     (define df1 (df-read/csv sample-csv))
     (check equal? (sort (df-series-names df1) string<?) '("four" "one" "three" "two"))
     (check = (df-row-count df1) 13)
     (check equal? (df-select* df1 "one" "two" "three" "four")
            #(#(1 2 3 4)
              #(4 5 6 "abc")
              #(7 8 9 "def,gh")
              #(10 11 12 "a,bc 123 d\"ef")
              #(14 15 #f #f)
              #("16" "-17" #f #f)
              #("16.5" "1E3" #f #f)
              #("-1e-3" "7.5e-3" #f #f)
              #("-1e-2+3.5i" " -1e-2+3.5i " #f #f)
              #(16 -17 #f #f)
              #(16.5 1e3 #f #f)
              #(-1e-3 7.5e-3 #f #f)
              #(-1e-2+3.5i -1e-2+3.5i #f #f))))

   (test-case "df-read/csv: quoted-numbers"
     ;; Read the same file again, note that the last row will now contain
     ;; numbers, not strings
     (define df1a (df-read/csv sample-csv #:quoted-numbers? #t))
     (check equal? (sort (df-series-names df1a) string<?) '("four" "one" "three" "two"))
     (check = (df-row-count df1a) 13)
     (check equal? (df-select* df1a "one" "two" "three" "four")
            #(#(1 2 3 4)
              #(4 5 6 "abc")
              #(7 8 9 "def,gh")
              #(10 11 12 "a,bc 123 d\"ef")
              #(14 15 #f #f)
              #(16 -17 #f #f)
              #(16.5 1e3 #f #f)
              #(-1e-3 7.5e-3 #f #f)
              #(-1e-2+3.5i -1e-2+3.5i #f #f)
              #(16 -17 #f #f)
              #(16.5 1e3 #f #f)
              #(-1e-3 7.5e-3 #f #f)
              #(-1e-2+3.5i -1e-2+3.5i #f #f))))

   (test-case "read/custom-na-string"
     ;; This CSV file contains "-" in the "empty" cells, strip them out when
     ;; reading them.
     (let ((df (df-read/csv sample2-csv #:na "-")))
       (check > (df-count-na df "two") 0)))

   (test-case "read/empty-strings"
     ;; This CSV contains empty quoted strings. They should be read in as NA
     ;; values.
     (let ((df (df-read/csv sample3-csv)))
       (check = (df-count-na df "one") 1)
       (check = (df-count-na df "two") 1)))

   (test-case "read/na-proc"
     ;; Use a custom NA function which treats even numbers as NA.
     (let ((df (df-read/csv sample3-csv
                            #:na (lambda (v)
                                   (let ((v (if (string? v) (string->number v) v)))
                                     (and (number? v) (even? v)))))))
       (check = (df-count-na df "one") 4)
       (check = (df-count-na df "two") 2)))

   (test-case "df-write/csv: other"
     (define df (make-data-frame))

     (define s1 (make-series "s,1" #:data #(1 1/2 3 #f 5) #:na #f))
     (define s2 (make-series "s,2" #:data #("one" "two" "th\"ree" #f "") #:na ""))
     (df-add-series! df s1)
     (df-add-series! df s2)

     (define text (call-with-output-string
                   (lambda (out) (df-write/csv df out))))
     (check equal? text "\"s,1\",\"s,2\"\n1,\"one\"\n0.5,\"two\"\n3,\"th\"\"ree\"\n,\"#f\"\n5,\n")

     (define text2 (call-with-output-string
                    (lambda (out) (df-write/csv df out "s,1" #:start 1 #:stop 3))))
     (check equal? text2 "\"s,1\"\n0.5\n3\n")

     ;; Try writing it out to file and reading from an input port, this is a
     ;; slightly different code path than writing to an output port.
     (check-not-exn
      (lambda ()
        (df-write/csv df csv-test-file)))
     (check-not-exn
      (lambda ()
        (call-with-input-string text (lambda (in) (df-read/csv in))))))



   (test-case "df-read/csv: duplicate header names"
     (define df (df-read/csv sample4-csv))
     (check equal? (sort (df-series-names df) string<?) '("one" "one (1)" "one (2)" "one (3)"))
     (check equal? (df-select df "one") #(1 1))
     (check equal? (df-select df "one (1)") #(2 2))
     (check equal? (df-select df "one (2)") #(3 3))
     (check equal? (df-select df "one (3)") #(4 4)))))




(define gpx-tests
  (test-suite
   "df-read+write/gpx"
   #:after (lambda ()
             (with-handlers (((lambda (e) #t) (lambda (e) #t)))
               (delete-file gpx-test-file)))

   (test-case "df-read/gpx: basic"
     (define df (df-read/gpx sample-gpx-file))
     (check-true (df-contains? df "lat" "lon" "alt" "dst" "timestamp"))
     (check > (df-row-count df) 0))

   (test-case "df-read+write/gpx: other"

     (define df-1136 (df-read/csv sample-1136-file))
     (check-true (df-contains? df-1136 "lat" "lon" "alt" "timestamp"))
     (check > (df-row-count df-1136) 0)
     (df-write/gpx df-1136 gpx-test-file)
     (define df-1136-2 (df-read/gpx gpx-test-file))

     ;; Check that we read back what we wrote out...
     (for (([lat1 lon1 alt1 ts1] (in-data-frame df-1136 "lat" "lon" "calt" "timestamp"))
           ([lat2 lon2 alt2 ts2] (in-data-frame df-1136-2 "lat" "lon" "alt" "timestamp")))
       (check < (abs (- lat1 lat2)) 1e-5)
       (check < (abs (- lon1 lon2)) 1e-5)
       (check < (abs (- alt1 alt2)) 1e-5)
       (check < (abs (- ts1 ts2)) 1e-5))

     ;; Try writing and reading from an output port, this is a slightly
     ;; different code path from reading and writing to file...
     (define str #f)
     (check-not-exn
      (lambda ()
        (set! str (call-with-output-string
                   (lambda (out) (df-write/gpx df-1136 out))))))
     (check-not-exn
      (lambda ()
        (call-with-input-string str (lambda (in) (df-read/gpx in))))))))



(define tcx-tests
  (test-suite
   "df-read/tcx"

   (test-case "df-read/tcx: basic"
     (define df (df-read/tcx sample-tcx-file))
     (check-true (df-contains? df "alt" "cad" "dst" "lat" "lon" "pwr" "spd" "timestamp"))
     (check-true (> (df-row-count df) 0))
     (check-true (list? (df-get-property df 'laps)))
     (check-true (> (length (df-get-property df 'laps))))
     (check-true (number? (df-get-property df 'unit-id)))
     (check-true (number? (df-get-property df 'product-id)))
     (check-true (string? (df-get-property df 'tcx-sport))))

   (test-case "df-read/tcx/multiple: basic"
     (define dfs (df-read/tcx/multiple sample-tcx-file))
     (check-true (= (length dfs) 1))
     (define df (first dfs))
     (check-true (df-contains? df "alt" "cad" "dst" "lat" "lon" "pwr" "spd" "timestamp"))
     (check-true (> (df-row-count df) 0))
     (check-true (list? (df-get-property df 'laps)))
     (check-true (> (length (df-get-property df 'laps)))))))


(define stats+mmax-tests
  (test-suite
   "statistics+meanmax"
   (test-case "statistics + meanmax test cases"

     (define df (df-read/csv sample-1136-file))
     (check-not-exn
      (lambda ()
        (df-set-default-weight-series! df #f)))

     (define s (df-statistics df "spd"))
     (check < (abs (- (statistics-mean s) 0.88)) 1e-2)

     ;; This session data contains data that is sampled at irregular
     ;; intervals.  A simple average (where the weight series is #f) will not
     ;; be correct, since each sample "counts" a variable amount to the total.
     ;; Using the "timer" series as the weight is the correct way to calculate
     ;; average speed, using the "dst" series as the weight is the correct way
     ;; to calculate average pace, see also
     ;; https://github.com/alex-hhh/ActivityLog2/issues/17
     ;;
     ;; If data would have been sampled at regular intervals, a simple
     ;; unweighted average would do.

     (check-not-exn
      (lambda ()
        (df-set-default-weight-series! df "timer")))

     (define w (df-statistics df "spd"))
     (check < (abs (- (statistics-mean w) 0.81)) 1e-2)

     ;; TODO: need better tests for the quantile.  This really tests that we
     ;; can run the function in its basic form

     (define q (df-quantile df "spd"))
     (check equal? (length q) 5)

     (check-not-exn
      (lambda()
        (define mmax (df-mean-max df "spd"))
        (check > (length mmax) 0))))))       ; need a better test





(define histogram-tests
  (test-suite
   "df-histogram"
   (test-case "df-histogram: other"

     (define df-1136 (df-read/csv sample-1136-file))

     (let ((h (df-histogram df-1136 "spd" #:bucket-width 1)))
       (check = (vector-length h) 3)
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136))))

     (let ((h (df-histogram df-1136 "spd" #:bucket-width 0.01)))
       (check > (vector-length h) 1)
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136))))

     ;; Add a string series classifying the heart rate.  We get a histogram of
     ;; this classification and check that too...
     (df-add-derived! df-1136 "spd-tag" '("spd")
                      (lambda (val)
                        (match-define (list spd) val)
                        (cond ((< spd 1.0) "low")
                              ((> spd 1.1) "high")
                              (#t "med"))))
     (let ((h (df-histogram df-1136 "spd-tag")))
       (check = (vector-length h) 3)      ; the three tags
       (let ((n (for/sum ([item (in-vector h)]) (vector-ref item 1))))
         (check = n (df-row-count df-1136)))))))




(define rdp-simplify-tests
  (test-suite
   "rdp-simplify"

   ;; Degenerate cases for 0, 1, 2 vector sizes.  In these cases, the
   ;; function should just return the input vector
   (test-case "rdp-simplify: 0 elements"
     (define vzero (vector))
     (define result (rdp-simplify vzero))
     (check = (vector-length result) 0))

   (test-case "rdp-simplify: 1 element"
     (define vone (vector (vector 0 1)))
     (define result (rdp-simplify vone))
     (check = (vector-length result) 1)
     (check equal? (vector-ref result 0) (vector 0 1)))

   (test-case "rdp-simplify: 2 elements"
     (define vtwo (vector (vector 0 1) (vector 0 2)))
     (define result (rdp-simplify vtwo))
     (check = (vector-length result) 2)
     (check equal? (vector-ref result 0) (vector 0 1))
     (check equal? (vector-ref result 1) (vector 0 2)))

   (test-case "rdp-simplify: other"

     ;; NOTE: it would be nice if we could test that rdp-simplify actually did
     ;; a sane simplification, rather than just reduce the number of points...

     (define df-1136 (df-read/csv sample-1136-file))
     (define data (df-select* df-1136 "timer" "spd"))

     (define data-1 (rdp-simplify data #:epsilon 0.01))
     (define data-2 (rdp-simplify data #:epsilon 0.02))
     (define data-3 (rdp-simplify data #:epsilon 0.03))

     (check < (vector-length data-2) (vector-length data-1))
     (check < (vector-length data-3) (vector-length data-2))

     (define nitems (vector-length data))
     (define test-point (exact-truncate (/ nitems 2))) ; midway point
     (define data-5 (rdp-simplify data #:epsilon 0.03
                                  #:keep-positions
                                  (list test-point
                                        (sub1 nitems) ; last one
                                        (+ nitems 5)))) ; out of range

     ;; The test-point and the one that follows were kept in the output set...
     (check-pred exact-nonnegative-integer?
                 (vector-memq (vector-ref data test-point) data-5))
     (check-pred exact-nonnegative-integer?
                 (vector-memq (vector-ref data (add1 test-point)) data-5))

     (define data-4 (rdp-simplify data #:epsilon 0.04 #:destroy-original? #t))
     (check < (vector-length data-4) (vector-length data-3))
     ;; data now contains #f values, as it was destroyed
     (check > (for/sum ([d (in-vector data)] #:when (eq? #f d)) 1) 0))))



(define scatter-tests
  (test-suite
   "scatter"
   (test-case "time-delay-series"

     (define timestamp 1000000)
     (define nitems 100)
     (define shift-amount 20)

     (define tdata
       (for/vector #:length nitems
           ([i (in-range nitems)])
         (vector i i (+ timestamp i))))

     ;; We expect less items in the delayed series, as items will be dropped
     ;; from the end or the start...
     (define expected-length (- nitems shift-amount))

     (define delay-positive (time-delay-series tdata shift-amount))
     (check = (vector-length delay-positive) expected-length)
     (for ((item (in-vector delay-positive)))
       (match-define (vector x y ts) item)
       ;; NOTE: the x y and timestamp values were chosen carefully so we can
       ;; check if they shifted correctly with simple arithmetic!
       (check = (- ts x) timestamp)
       (check = (- y x) shift-amount))

     (define delay-negative (time-delay-series tdata (- shift-amount)))
     (check = (vector-length delay-negative) expected-length)
     (for ((item (in-vector delay-negative)))
       (match-define (vector x y ts) item)
       ;; NOTE: the x y and timestamp values were chosen carefully so we can
       ;; check if they shifted correctly with simple arithmetic!
       (check = (- ts x) timestamp)
       (check = (- y x) (- shift-amount))))))



;;.................................................... least-squares-fit ....

;; Check that the fit returned by `df-least-squares-fit` is indeed a best fit
;; -- we test that by modifying the coefficients one by one and see if the
;; residual from the modified fit function is bigger than the original. If it
;; is smaller, we just found a better fit than `df-least-squares-fit`
;; returned!
(define (check-modified-residuals fit df xseries yseries)

  (define (make-polynomial coefficients)
    (lambda (x)
      (define-values (y p)
        (for/fold ([y 0] [p 1])
                  ([c (in-list coefficients)])
          (values (+ y (* p c)) (* p x))))
      y))

  (define (make-exponential coefficients)
    (match-define (list a b c) coefficients)
    (lambda (x) (+ (* a (exp (* b x))) c)))

  (define (make-logarithmic coefficients)
    (match-define (list a b) coefficients)
    (lambda (x) (+ a (* b (log x)))))

  (define (make-power-law coefficients)
    (match-define (list a b) coefficients)
    (lambda (x) (* a (expt x b))))

  (define (calculate-residual fn)
    (for/sum (([x y] (in-data-frame df xseries yseries)))
      (define d (- y (fn x)))
      (* d d)))

  (define (scale-coeffient coefficients n factor)
    (let ((item (list-ref coefficients n))
          (head (take coefficients n))
          (tail (drop coefficients (add1 n))))
      (append head (list (* item factor)) tail)))

  (match-define (least-squares-fit type coefficients residual fn) fit)
  (for ([n (in-range (length coefficients))])
    (for ([factor (in-list '(0.95 1.05))])
      (define ncoeff (scale-coeffient coefficients n factor))
      (define nfn
        (case type
          ((linear polynomial) (make-polynomial ncoeff))
          ((exponential) (make-exponential ncoeff))
          ((logarithmic) (make-logarithmic ncoeff))
          ((power) (make-power-law ncoeff))
          (else (lambda (x) 1))))
      (define nresidual (calculate-residual nfn))
      (check > nresidual residual))))

(define least-squares-fit-tests
  (test-suite
   "df-least-squares-fit"
   (test-case "df-least-squares-fit: other"

     (define df (df-read/csv lsq-test-file))

     (define fit-linear (df-least-squares-fit
                         df "base" "linear"
                         #:mode 'linear
                         #:residual? #t))
     (check-modified-residuals fit-linear df "base" "linear")

     (define fit-first (df-least-squares-fit
                        df "base" "linear"
                        #:mode 'polynomial
                        #:polynomial-degree 1
                        #:residual? #t))
     (check-modified-residuals fit-first df "base" "linear")

     (define fit-second (df-least-squares-fit
                         df "base" "second"
                         #:mode 'polynomial
                         #:polynomial-degree 2
                         #:residual? #t))
     (check-modified-residuals fit-second df "base" "second")

     (define fit-third (df-least-squares-fit
                        df "base" "third"
                        #:mode 'polynomial
                        #:polynomial-degree 3
                        #:residual? #t))
     (check-modified-residuals fit-third df "base" "third")

     ;; NOTE: exponential fit does not seem to generate minimum residuals, not
     ;; a mathematician, so I don't know why, see explanation for
     ;; `df-least-squares-fit`.

     ;; (define fit-exp (df-least-squares-fit
     ;;                  df "base" "exp"
     ;;                  #:mode 'exponential
     ;;                  #:residual? #t
     ;;                  #:annealing? #t
     ;;                  #:annealing-iterations 1000))
     ;; (check-modified-residuals fit-exp df "base" "exp")

     (define fit-log (df-least-squares-fit
                      df "base2" "log"
                      #:mode 'logarithmic
                      #:residual? #t))
     (check-modified-residuals fit-log df "base2" "log"))))

     ;; NOTE: power fit does not seem to generate minimum residuals, not a
     ;; mathematician, so I don't know why, see explanation for
     ;; `df-least-squares-fit`.
     ;;
     ;; (define fit-pow (df-least-squares-fit
     ;;                  df "base2" "pow"
     ;;                  #:mode 'power
     ;;                  #:residual? #t
     ;;                  #:annealing? #t
     ;;                  #:annealing-iterations 1000))
     ;; (check-modified-residuals fit-pow df "base2" "pow")



(define slr-tests
  (test-suite
   "slr"
   (test-case "AB#26 no slr"
     ;; No linear regression can be generated if the XS series has a 0
     ;; standard deviation (i.e. all values are the same)
     (check-equal? (simple-linear-regression '(1 1 1) '(1 2 3)) #f))))

(define for-tests
  (test-suite
   "for/data-frame"
   (test-case "for/data-frame: nothing to iterate on"
     (define for-nothing
       (for/data-frame (col1 col2 col3)
                       ([v (in-list '())])
         (values v v v)))
     (define for*-nothing
       (for*/data-frame (col1 col2 col3)
                        ([v (in-list '())]
                         [v2 (in-list '())]
                         [v3 (in-list '())])
         (values v v2 v3)))

     (check-equal? (list "col1" "col2" "col3")
                   (sort (df-series-names for-nothing) string-ci<?))
     (check-equal? (list "col1" "col2" "col3")
                   (sort (df-series-names for*-nothing) string-ci<?))
     (check-equal? (df-row-count for-nothing) 0)
     (check-equal? (df-row-count for*-nothing) 0))

   (test-case "for/data-frame: something to iterate on"
     (define for-something
       (for/data-frame (ints strs)
                       ([v (in-range 5)]
                        [v2 (in-list (list "a" "b" "c"))])
         (values v v2)))
     (define for*-something
       (for*/data-frame (ints strs)
                        ([v (in-range 5)]
                         [v2 (in-list (list "a" "b" "c"))])
         (values v v2)))

     (check-equal? (df-row-count for-something) 3)
     (check-equal? (df-select for-something "ints")
                   (vector 0 1 2))
     (check-equal? (df-select for-something "strs")
                   (vector "a" "b" "c"))

     (check-equal? (df-row-count for*-something) 15)
     (check-equal? (df-select for*-something "ints")
                   (vector 0 0 0 1 1 1 2 2 2 3 3 3 4 4 4))
     (check-equal? (df-select for*-something "strs")
                   (vector "a" "b" "c" "a" "b" "c" "a" "b" "c"
                           "a" "b" "c" "a" "b" "c")))))


;;................................................................. rest ....

(module+ test
  (require al2-test-runner)
  (run-tests #:package "data-frame"
             #:results-file "test-results-data-frame.xml"
             lower+upper-bound-tests
             series-tests
             sx-tests
             spline-tests
             df-tests
             dfdb-tests
             csv-tests
             gpx-tests
             tcx-tests
             stats+mmax-tests
             histogram-tests
             rdp-simplify-tests
             scatter-tests
             least-squares-fit-tests
             slr-tests
             for-tests))
