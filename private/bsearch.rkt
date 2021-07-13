#lang racket/base

;; bsearch.rkt -- binary search in a sorted vector
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

(require racket/contract
         racket/math)

;; Both `lower-bound` and `upper-bound` will search a sorted vector, VEC for a
;; value VAL and return an index into the vector.  The vector is assumed to
;; contain sorted values, as defined by CMP-FN (which must define a strict
;; ordering, that is a "less than" operation, BUT NOT a "less than or equal"
;; operation).  KEY, if present, selects the value to compare (useful if the
;; vector contains structures and we want to search on a structure slot).
;; START and END define the sub-range of the vector to search.
;;
;; If unsure, you need to use `lower-bound`
;;
;; `lower-bound` will return an index identifying the earliest position where
;; VAL could be inserted to keep the range sorted, while upper-bound will
;; return the last position where an insertion would keep the value sorted.
;;
;; * if VAl exists one or more times in the vector, `lower-bound` will return
;; the first occurrence, but `upper-bound` will return one after the last
;; occurrence.  For example:
;;
;;     (define data (vector 1 2 3 4 5))
;;     (lower-bound data 3) => 2 (position of '3' is at index 2)
;;     (upper-bound data 3) => 3
;;
;;     (define data (vector 1 2 3 3 3 4 5))
;;     (lower-bound data 3) => 2 (first '3' is at index 2)
;;     (upper-bound data 3) => 5 (NOTE: at index 5 we have the value '4')
;;
;; * if VAL is smaller than the first value in the range, both functions
;; return START
;;
;; * if VAL is greater than the last value in the range, both functions return
;; END (this is considered out of range for the vector)
;;
;; * If VAL does not exist, an index is returned representing the location of
;; VAL in the vector (or the "best" location, if val is not found).  Same
;; index is returned by both functions.
;;
;; To determine if a value actually exists in a vector you also need to check
;; the actual value at the position returned by `lower-bound`, like so:
;;
;; (define (exists? data v)
;;   (define index (lower-bound data v))
;;   (and (< index (vector-length data))
;;        (equal? v (vector-ref data index))))
;;
;; NOTE: this works like the std::lower_bound() and std::upper_bound()
;; functions in C++.

(define (lower-bound vec val
                     #:cmp (cmp-fn <)
                     #:key (key-fn values)
                     #:start (start 0)
                     #:stop (end (vector-length vec)))

  (let ((vlen (vector-length vec)))
    (cond ((or (< start 0) (> start vlen))
           (raise-range-error 'lower-bound "vector" "starting " start vec 0 vlen))
          ((or (< end 0) (> end vlen))
           (raise-range-error 'lower-bound "vector" "ending " end vec 0 vlen))
          ((> start end)
           (raise-range-error
            'lower-bound "vector" "ending " end vec start vlen 0))))

  (let loop ([start start]
             [end end])
>>>>>>> upstream/ah/lower-bound
    (if (= start end)
        start
        (let* ((mid (exact-truncate (/ (+ start end) 2)))
               (mid-val (key-fn (vector-ref vec mid))))
          (if (cmp-fn mid-val val)
              (loop (add1 mid) end)
              (loop start mid))))))

define (upper-bound vec val
                     #:cmp (cmp-fn <)
                     #:key (key-fn values)
                     #:start (start 0)
                     #:stop (end (vector-length vec)))

  (define (do-reverse-search start end)
    ; retain equality or else we end up with issues
    (define (not-cmpfn a b)
      ; if cmp-fn and equal? are true, this predicate respects equal?, and its negation
      ; should too. (like <= and >=)
      (define respect-equal? (and (equal? a b) (cmp-fn a b)))
      (or (and respect-equal? (equal? a b))
          (not (cmp-fn a b))))

    (if (= start end)
        (sub1 start)
        (let* ((mid (exact-truncate (/ (+ start end) 2)))
               (mid-val (key-fn (vector-ref vec mid))))
          (if (not-cmpfn val mid-val)
              (do-reverse-search (add1 mid) end)
              (if (not-cmpfn mid-val val)
                  (do-reverse-search start mid)
                  mid)))))

  (let ((vlen (vector-length vec)))
    (cond ((or (< start 0) (> start vlen))
           (raise-range-error 'upper-bound "vector" "starting " start vec 0 vlen))
          ((or (< end 0) (> end vlen))
           (raise-range-error 'upper-bound "vector" "ending " end vec 0 vlen))
          ((> start end)
           (raise-range-error
            'upper-bound "vector" "ending " end vec start vlen 0))))

  (let loop ([start start]
             [end end])
    (if (= start end)
        start
        (let* ((mid (exact-truncate (/ (+ start end) 2)))
               (mid-val (key-fn (vector-ref vec mid))))
          (if (cmp-fn val mid-val)
              (loop start mid)
              (loop (add1 mid) end)))))

;; Return two values representing the start and one-plus end ranges where VAL
;; is present in the sorted vector VEC.  This is equivalent to calling
;; lower-bound and upper-bound with the same parameters, but will run somewhat
;; faster
(define (equal-range vec val
                     #:cmp (cmp-fn <)
                     #:key (key-fn values)
                     #:start (start 0)
                     #:stop (end (vector-length vec)))

  (let ((vlen (vector-length vec)))
    (cond ((or (< start 0) (> start vlen))
           (raise-range-error 'equal-range "vector" "starting " start vec 0 vlen))
          ((or (< end 0) (> end vlen))
           (raise-range-error 'equal-range "vector" "ending " end vec 0 vlen))
          ((> start end)
           (raise-range-error
            'equal-range "vector" "ending " end vec start vlen 0))))
  
  (define lb (lower-bound vec val #:cmp cmp-fn #:key key-fn #:start start #:stop end))
  (cond ((>= lb end) (values lb lb))
        ((equal? (vector-ref vec lb) val)
         (values lb
                 (upper-bound vec val #:cmp cmp-fn #:key key-fn #:start lb #:stop end)))
        (#t
         (values lb lb))))


;;............................................................. provides ....

(provide/contract
 (lower-bound (->* ((vectorof any/c) any/c)
                   (#:cmp (-> any/c any/c boolean?)
                    #:key (-> any/c any/c)
                    #:start integer?
                    #:stop integer?)
                   integer?))

 (upper-bound (->* ((vectorof any/c) any/c)
                   (#:cmp (-> any/c any/c boolean?)
                    #:key (-> any/c any/c)
                    #:start integer?
                    #:stop integer?)
                   integer?))

 (equal-range (->* ((vectorof any/c) any/c)
                   (#:cmp (-> any/c any/c boolean?)
                    #:key (-> any/c any/c)
                    #:start integer?
                    #:stop integer?)
                   (values integer? integer?))))

