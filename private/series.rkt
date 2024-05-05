#lang racket/base

;; series.rkt -- data frame series and related functions
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018, 2021, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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
         racket/match
         racket/math
         racket/vector
         "bsearch.rkt"
         "exn.rkt")

;; A series represents a single column of data in a data frame.  It has a name
;; and holds the actual values in a racket vector.  A series can also have:
;;
;; * a compare function, if preset, the data in the vector is assumed to be
;; sorted according to this function, and updates will have to preserve the
;; sort order.
;;
;; * a contract function, if present, all elements in the series have to
;; satisfy this contract.
;;
;; * a NA value, which denotes the "not available" value for elements in the
;; series.
;;
(struct series
  (name                          ; name of this column
   data                          ; a vector containing column data
   end                           ; end index in the vector where data ends
   cmpfn                         ; a compare function, when present, all
   ; elements in this column must be sorted
   ; according to this function
   na                            ; value for the Not Available slots
   contractfn                    ; when not #f, specified contract for elements
   )
  #:mutable)


;; Check that the series C is sorted according to CMPFN, and raise an error if
;; it is not.
(define (check-valid-sort c cmpfn)
  (match-define (series name data end _ na _) c)
  (for ((index (in-range 0 end)))
    (define v (vector-ref data index))
    (define vprev (if (> index 0) (vector-ref data (sub1 index)) #f))
    (cond ((equal? v na)
           (df-raise "check-valid-sort: ~a contains NA / values @~a" name index))
          ((and (> index 0)
                ;; NOTE: CMPFN defines strict ordering (i.e less than).  We
                ;; admit equal values by assuming that either a < b or (not b
                ;; < a), this is better than using equal? since (equal? 0 0.0)
                ;; is #f...
                (not (or (cmpfn vprev v) (not (cmpfn v vprev)))))
           (df-raise "check-valid-sort: ~a not really sorted @~a (~a vs ~a)"
                     name index vprev v)))))

;; Check that all the values in the series C match the contract function
;; CONTRACTFN, and raise an error if they are not.
(define (check-valid-contract c contractfn)
  (match-define (series name data end _ na _) c)
  (for ((index (in-range 0 end)))
    (define v (vector-ref data index))
    (unless (or (equal? v na) (contractfn v))
      (df-raise "check-valid-contract: ~a value ~a fails contract @~a" name v index))))

;; Check that inserting VAL at INDEX is valid: if there is a contract
;; specified, we check that VAL satisfies the contract, if the series is
;; sorted, check that the sort order is preserved.  An error is raised if
;; there is a problem with inserting this value.
(define (check-valid-insert c index val)
  (match-define (series _ data end cmpfn na contractfn) c)
  (when (or (< index 0) (> index end))
    (raise-range-error
     'check-valid-insert "vector" "" index data 0 end))
  ;; Check for contract match
  (when (and contractfn (not (equal? val na)) (not (contractfn val)))
    (raise-argument-error 'check-valid-insert
                          "val"
                          "contract"
                          val))
  (when cmpfn
    (when (equal? na val)
      (raise-argument-error
       'check-valid-insert
       "value cannot be NA in a sorted column"
       val))
    (let ((prev-index (if (> index 0) (sub1 index) #f)))
      (when prev-index
        (unless (cmpfn (vector-ref data prev-index) val)
          (raise-argument-error
           'check-valid-insert
           "not sorted w.r.t. previous element"
           val))))
    (let ((next-index (if (< index (sub1 end)) (add1 index) #f)))
      (when next-index
        (unless (cmpfn val (vector-ref data next-index))
          (raise-argument-error
           'check-valid-insert
           "not sorted w.r.t. next element"
           val))))))

;; Make sure there is enough room in the series C for adding at least "SPACE"
;; elements.  A new storage vector will be allocated if necessary, but the
;; number of actual elements in the series will not change.
(define (series-reserve-space c space)
  (when (< (series-free-space c) space)
    (match-define (series name data end _ na _) c)
    (define ncap (max (exact-truncate (* (vector-length data) 1.5))
                      (+ end space)))
    (define ndata (make-vector ncap na))
    (for (([v idx] (in-indexed (in-vector data 0 end))))
      (vector-set! ndata idx v))
    (set-series-data! c ndata)
    (set-series-end! c end)))

;; Construct a new data series.  NAME is the name of the data series.  DATA is
;; a data vector with the contents of the series (if #f, an empty series is
;; constructed).  CAPACITY specifies the initial capacity of an empty series
;; (i.e. how many elements can be added before a new vector needs to be
;; allocated) -- this is intended as an optimization.  NA specifies the "not
;; available" value for the series.  CMPFN specifies a compare function -- if
;; present, the series will have to be sorted according to this function.
;; CONTRACT, if present, is a function which validates the elements in the
;; series.
(define (make-series name
                     #:data (data #f)
                     #:capacity (capacity 10)
                     #:cmpfn (cmpfn #f)
                     #:na (na #f)
                     #:contract (contractfn #f))
  (define df
    (series
     name
     (or data (make-vector capacity na))
     (if data (vector-length data) 0)
     cmpfn
     na
     contractfn))
  (when contractfn
    (check-valid-contract df contractfn))
  (when cmpfn
    (check-valid-sort df cmpfn))
  df)

;; Creates a copy of the series C.
(define (copy-series c)
  (struct-copy series c [data (vector-copy (series-data c))]))

;; Return the number of elements in the series C.
(define (series-size c)
  (series-end c))

;; Return the amount of free space in the series C.  This represents the
;; number of elements that can be added to the series without triggering a new
;; allocation.
(define (series-free-space c)
  (- (vector-length (series-data c)) (series-end c)))

;; Return #t if the series C is empty
(define (series-empty? c)
  (= (series-size c) 0))

;; Return the value at INDEX in the series C
(define (series-ref c index)
  (match-define (series name data end _ _ _) c)
  (when (or (< index 0) (>= index end))
    (raise-range-error
     'series-ref (format "vector(~a)" name) "" index data 0 end))
  (vector-ref data index))

;; Unsafe version of `series-ref` -- does not check bounds.
(define (unsafe-series-ref c index)
  (vector-ref (series-data c) index))

;; Update the value at INDEX in the series C to the new value VAL.  If a CMPFN
;; or CONTRACTFN is present in the series, value has to satisfy the contract
;; and sort order respectively, otherwise, an error is raised.
(define (series-set! c index val)
  (match-define (series name data end _ _ contractfn) c)
  (when (or (< index 0) (>= index end))
    (raise-range-error
     'series (format "vector(~a)" name) "" index data 0 end))
  (check-valid-insert c index val)
  ;; When the series is passed a #:data argument, we might get an immutable
  ;; vector, make a mutable one, so we can set a value in it (we delay doing
  ;; this, so we avoid unnecessary copying if the data never changes.
  (when (immutable? data)
    (set! data (vector-copy data)))
  (set-series-data! c data)
  (vector-set! data index val))

;; Mark the data series C as sorted according to CMPFN.  This will not sort
;; the series, but it will validate all elements and raise an error if the
;; series is not actually sorted.  If CMPFN is #f, the sort restriction is
;; removed from the series.
(define (series-set-sorted! c cmpfn)
  (when cmpfn
    (check-valid-sort c cmpfn))
  (set-series-cmpfn! c cmpfn))

;; Checks if the given series C is sorted (so it has a comparison function,
;; as if it had one and was not sorted, this would be a construction time
;; error).
(define (series-is-sorted? c)
  (and (series-cmpfn c) #t))

;; Mark the data series C as having the CONTRACTFN contract.  All elements
;; will be validated with the contract first and an error will be raised if
;; they don't satisfy the contract.
(define (series-set-contract! c contractfn)
  (when contractfn
    (check-valid-contract c contractfn))
  (set-series-contractfn! c contractfn))

;; Append a new element, VAL, to the data series C.  If a contract is present,
;; VAL has to satisfy the contract.  If the series is sorted, VAL has to
;; preserve the sort order.
(define (series-push-back! c val)
  (check-valid-insert c (series-end c) val)
  (series-reserve-space c 1)
  (vector-set! (series-data c) (series-end c) val)
  (set-series-end! c (add1 (series-end c))))

;; Like series-push-back, but don't check that the element is valid.
(define (unsafe-series-push-back! c val)
  (series-reserve-space c 1)
  (vector-set! (series-data c) (series-end c) val)
  (set-series-end! c (add1 (series-end c))))

;; Return a sequence that enumerates elements in the data series C between
;; START and STOP with STEP.  This can be used in for loops.
(define (in-series c [start 0] [stop (series-size c)] [step 1])
  (match-define (series _ data end _ _ _) c)
  (define s start)
  (define e stop)
  (if (<= start stop)
      (begin
        (unless (<= 0 s end)
          (raise-range-error
           'in-series "vector1" "" start data 0 end))
        (unless (<= 0 e end)
          (raise-range-error
           'in-series "vector2" "" stop data 0 end))
        (unless (> step 0)
          (raise-argument-error 'step "positive" step)))
      (begin
        (unless (<= -1 s end)
          (raise-range-error
           'in-series "vector3" "" start data 0 end))
        (unless (<= -1 e end)
          (raise-range-error
           'in-series "vector4" "" stop data 0 end))
        (unless (< step 0)
          (raise-argument-error 'step "negative" step))))
  (in-vector (series-data c) s e step))

;; Find the index of VALUE in the data series C.  The series has to be sorted,
;; otherwise an error is raised.  If the exact value is not found in the
;; series, the index of the first value greater than VALUE is returned.
;; I.e. VALUE will have to be inserted at or before the returned index to keep
;; the series sorted.  Will return (series-size c) if the searched value is
;; greater than all values in the series, note that this is not a valid index.
(define (series-index-of c value #:exact-match? (exact-match? #f))
  (match-define (series name data end cmpfn _ _) c)
  (if cmpfn
      (let ([pos (unsafe-lower-bound data value #:cmp cmpfn #:start 0 #:stop end)])
        (if exact-match?
            (and (< pos end) (equal? (vector-ref data pos) value) pos)
            pos))
      (df-raise (format "series-index-of: ~a is not sorted" name))
      #;(for/first ([(x index) (in-indexed (in-vector data 0 end))]
                    #:when (equal? x value))
          index)))

(define (series-all-indices-of c value)
  (match-define (series name data end cmpfn _ _) c)
  (if cmpfn
      (let-values ([(low high)
                    (equal-range data value #:cmp cmpfn #:start 0 #:stop end)])
        (if (> low (series-size c))
            '()
            (for/list ([idx (in-range low high)])
              idx)))
      (df-raise (format "series-index-of: ~a is not sorted" name))))

;; Find the first and last indices (multiple) of VALUE in the data series C.
;; The series has to be sorted, otherwise an error is raised. All other results
;; are the same as series-index-of.
(define (series-equal-range c value)
  (match-define (series name data end cmpfn _ _) c)
  (if cmpfn
      (equal-range data value #:cmp cmpfn #:start 0 #:stop end)
      (df-raise (format "series-equal-range: ~a is not sorted" name))))

;; Return the number of "not available" values in the data series.
(define (series-na-count c)
  (match-define (series _ data end _ na _) c)
  (or (for/sum ([x (in-vector data 0 end)] #:when (equal? x na)) 1) 0))

;; Return #t if the data series C has any "not available" values.  This is the
;; same as (eqv? (series-na-count c) 0), but it is faster if there are any NA
;; values.
(define (series-has-na? c)
  (match-define (series _ data end _ na _) c)
  (for/first ([x (in-vector data 0 end)] #:when (equal? x na)) #t))

;; Return #t if the data series C has at least one value that is NOT "NA".
(define (series-has-non-na? c)
  (match-define (series _ data end _ na _) c)
  (for/first ([x (in-vector data 0 end)] #:unless (equal? x na)) #t))

;; Return true if VAL is the same as the NA value for the data series C.
(define (series-is-na? c val)
  (equal? (series-na c) val))


;;............................................................. provides ....

(provide/contract
 (make-series (->* (string?)
                   (#:data (or/c vector? #f)
                    #:capacity exact-nonnegative-integer?
                    #:cmpfn (or/c #f (-> any/c any/c boolean?))
                    #:na any/c
                    #:contract (-> any/c boolean?))
                   series?))
 (copy-series (-> series? series?))
 (series? (-> any/c boolean?))
 (series-na (-> series? any/c))
 (series-name (-> series? string?))
 (set-series-name! (-> series? string? any/c))
 (series-size (-> series? exact-nonnegative-integer?))
 (series-empty? (-> series? boolean?))
 (series-is-na? (-> series? any/c boolean?))
 (series-is-sorted? (-> series? boolean?))
 (series-free-space (-> series? exact-nonnegative-integer?))
 (series-reserve-space (-> series? exact-nonnegative-integer? any/c))
 (series-ref (-> series? exact-nonnegative-integer? any/c))
 (series-set! (-> series? exact-nonnegative-integer? any/c any/c))
 (series-push-back! (-> series? any/c any/c))
 (in-series (->* (series?) (exact-nonnegative-integer?
                            (or/c -1 exact-nonnegative-integer?)
                            exact-integer?)
                 sequence?))
 (series-index-of (->* (series? any/c)
                       (#:exact-match? boolean?)
                       (or/c #f exact-nonnegative-integer?)))
 (series-all-indices-of (-> series? any/c (listof exact-nonnegative-integer?)))
 (series-equal-range (-> series? any/c (values (or/c #f exact-nonnegative-integer?)
                                               (or/c #f exact-nonnegative-integer?))))
 (series-set-sorted! (-> series? (or/c #f (-> any/c any/c boolean?)) any/c))
 (series-set-contract! (-> series? (or/c #f (-> any/c boolean?)) any/c))
 (series-na-count (-> series? exact-nonnegative-integer?))
 (series-has-na? (-> series? boolean?))
 (series-has-non-na? (-> series? boolean?)))

(provide
 ;; Also provide it without a contract -- unsafe is unsafe
 unsafe-series-ref
 unsafe-series-push-back!)
