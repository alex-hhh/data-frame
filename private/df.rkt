#lang racket/base

;; df.rkt -- data frame implementation and basic routines
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018, 2021 Alex Harsányi <AlexHarsanyi@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be ul, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
;; License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/contract
         racket/match
         racket/vector
         "exn.rkt"
         "series.rkt"
         "bsearch.rkt")


;;........................................................... data-frame ....

;; A data frame holds together some data series in an unspecified order.  All
;; series in the data series have the same number of elements.  There are
;; methods for selecting a subset of the data, looking up values adding new
;; series, plus other things.
;;
;; A data series can also contain "properties" which are key,value pairs.
(struct data-frame
  (semaphore                 ; controls access to materializing delayed series
   [locking-thread #:mutable] ; the thread owning the semaphore
   series                    ; a hash table of series?
   delayed                   ; a hash table of functions which can create
   ; series (see `df-add-lazy`)
   properties                ; a hash table containing the series properties.
   secondary-indexes         ; a hash table of secondary indexes (see sx struct)
   ))

;; Construct an empty data frame
(define (make-data-frame)
  (data-frame
   (make-semaphore 1)
   #f
   (make-hash)
   (make-hash)
   (make-hash)
   (make-hash)))

;; Create a copy of the data frame DF.  The returned copy will reference the
;; same data series objects as the original (and the properties), but any
;; add/delete operations, for both series and properties, will only affect the
;; copy.
;;
;; Before copying the data frame, all lazy series will be materialized.
(define (df-shallow-copy df)
  (match-define (data-frame _ _ series delayed properties secondary-indexes) df)
  ;; Materialize all lazy series
  (for ([c (hash-keys delayed)])
    (df-get-series df c))
  (data-frame
   (make-semaphore 1)
   #f
   (hash-copy series)
   (make-hash)
   (hash-copy properties)
   (hash-copy secondary-indexes)))

;; Return a copy of the series named SERIES in the data frame DF. The copy
;; will not share anything with the original and can be modified
;; independently.
(define (df-duplicate-series df series)
  (let ([s (df-get-series df series)])
    (copy-series s)))

;; Return the series names in the data frame DF, as a list of strings.  The
;; names are returned in an unspecified order.
(define (df-series-names df)
  (match-define (data-frame _ _ series delayed _ _) df)
  (append (hash-keys series) (hash-keys delayed)))

;; Return #t if the data frame DF contains ALL the series specified in
;; SERIES-NAMES
(define (df-contains? df . series-names)
  (match-define (data-frame _ _ series delayed _ _) df)
  (for/and ([n (in-list series-names)])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

;; Return #t if the data frame DF contains ANY of the series specified in
;; SERIES-NAMES.
(define (df-contains/any? df . series-names)
  (match-define (data-frame _ _ series delayed _ _) df)
  (for/or ([n (in-list series-names)])
    (and (or (hash-ref series n #f)
             (hash-ref delayed n #f))
         #t)))

;; Return the series named NAME in the data frame DF.  The series is
;; materialized first, if needed -- i.e. if the series was added using
;; `df-add-lazy`, the thunk will be invoked to create the series.  An error is
;; raised if the series does not exist.
(define (df-get-series df name)
  (match-define (data-frame semaphore locking-thread series delayed _ _) df)
  (define should-unlock? #f)

  ;; Only lock the semaphore if it wasn't already locked it in the current
  ;; thread.  This is so 'get-series' can be recursively called in the current
  ;; thread and the delayed column can be materialized (as they usually depend
  ;; on other column.)
  (unless (eq? (current-thread) locking-thread)
    (semaphore-wait semaphore)
    (set! should-unlock? #t)
    (set-data-frame-locking-thread! df (current-thread)))

  (define (unlock)
    (when should-unlock?
      (set-data-frame-locking-thread! df #f)
      (set! should-unlock? #f)
      (semaphore-post semaphore)))

  (with-handlers
    (((lambda (e) #t)
      (lambda (e) (unlock) (raise e))))

    ;; Check if we have to materialize a delayed column first.
    (let ((thunk (hash-ref delayed name #f)))
      (when thunk
        (thunk)                         ; create the column now
        (hash-remove! delayed name)))

    (begin0
        (or (hash-ref series name #f)
            (df-raise "df-get-series (\"~a\"): no such series" name))
      (unlock))))

;; Returns the number of rows in the data frame DF.  All series inside a data
;; frame have the same number of rows.
(define (df-row-count df)
  (match-define (data-frame _ _ series _ _ _) df)
  (or (for/first ([v (in-hash-values series)])
        (series-size v))
      0))

;; Determines if the given SERIES in the data frame DF is sorted. Useful for
;; knowing when df-index-of is possible.
(define (df-is-sorted? df series)
  (let ([s (df-get-series df series)])
    (series-is-sorted? s)))

;; Return the value at INDEX for SERIES.
(define (df-ref df index series)
  (let ([s (df-get-series df series)])
    (series-ref s index)))

;; Set the VALUE at INDEX in SERIES.  This will first validate that the series
;; is still sorted (if SERIES was sorted) and that VALUE satisfies the SERIES
;; contract (if any).  The update will fail if these constraints are not
;; satisfied.
(define (df-set! df index value series)
  (invalidate-indices-using-series df series)
  (let ([s (df-get-series df series)])
    (series-set! s index value)))

;; Return a vector with values at INDEX for each element in the SERIES list.
(define (df-ref* df index . series)
  (for/vector ([series (in-list series)])
    (let ([s (df-get-series df series)])
      (series-ref s index))))

;; Return the NA value for SERIES.
(define (df-na-value df series)
  (define s (df-get-series df series))
  (series-na s))

;; Add SERIES to the data frame DF.  The new series must have the same number
;; of rows as existing series in the data frame, unless the data frame is
;; empty.
(define (df-add-series! df s)
  (match-define (data-frame _ _ series delayed _ _) df)
  ;; Check if the series has the same number of rows as the rest of the
  ;; series (if there are any other series in the data frame)
  (let ((nrows (df-row-count df))
        (name (series-name s))
        (size (series-size s)))
    (unless (or (hash-empty? series) (equal? nrows size))
      (df-raise "df-add-series! (\"~a\"): bad length ~a, expecting ~a" name size nrows))
    ;; If a lazy series was added by the same name, remove it...
    (invalidate-indices-using-series df name)
    (hash-remove! delayed name)
    (hash-set! series name s)))

;; Remove series NAME from the data frame, does nothing if the series does not
;; exist.
(define (df-del-series! df name)
  (drop-indices-using-series df name)
  (match-define (data-frame _ _ series delayed _ _) df)
  (hash-remove! series name)
  (hash-remove! delayed name))

;; Rename a series inside the data frame DF from OLD-NAME to NEW-NAME -- any
;; dependent structures will be updated too.  In particular this will
;; materialize any lazy series, making this a possibly costly operation.
(define (df-rename-series! df old-name new-name)
  (match-define (data-frame _ _ series delayed _ secondary-indexes) df)
  (define the-series
    (or (hash-ref series old-name #f)
        (df-raise "df-rename-series! (\"~a\"): no such series" old-name)))
  ;; Materialize all lazy series, in case one of them depends on OLD-NAME.
  (for ([c (hash-keys delayed)])
    (df-get-series df c))
  (define indices-using-old-name
    (indices-using-series df old-name))
  (hash-remove! series old-name)
  (set-series-name! the-series new-name)
  (hash-set! series new-name the-series)
  (for ([index (in-list indices-using-old-name)])
    (define nindex (sx-rename-series index old-name new-name))
    (hash-remove! secondary-indexes (sx-name index))
    (hash-set! secondary-indexes (sx-name nindex) nindex)))

;; Add a new series to the data frame whose values are computed from values of
;; existing series.  The data for the series is created using `df-map` by
;; applying VALUE-FN on BASE-SERIES and the new data is added to the data
;; frame.  See `df-map` for notes on the VALUE-FN.
(define (df-add-derived! df name base-series value-fn)
  (define data (df-map df base-series value-fn))
  (define col (make-series name #:data data))
  ;; NOTE: df-add-series! will invalidate any indices
  (df-add-series! df col))

;; Add a new series to the data frame, but delay creating it until it is
;; referenced.  This function allows adding many series to a data frame, with
;; the expectation that the cost to create those series is paid when (and if)
;; they are used.
(define (df-add-lazy! df name base-series value-fn)
  ;; If name is in base-series we'll get an infinite loop.
  (when (member name base-series)
    (df-raise "df-add-lazy! (\"~a\"): series is in base series: ~a" name base-series))
  ;; NOTE: there are no indices to invalidate, since this is a new series.
  (match-define (data-frame _ _ _ delayed _ _) df)
  (hash-set! delayed name
             (lambda ()
               (df-add-derived! df name base-series value-fn))))

;; Mark the SERIES as sorted according to CMPFN.  This does not actually sort
;; the data series, it just tells the data frame that the series can be used
;; for index lookup.  An error is raised if the series is not actually sorted
;; or if it contains NA values.
(define (df-set-sorted! df series cmpfn)
  (define col (df-get-series df series))
  (series-set-sorted! col cmpfn))

;; Set the contract for values in the data SERIES to CONTRACTFN.  An exception
;; is thrown if not all values in SERIES match contractfn or are NA (the
;; contractfn need not return #t for the NA value)
(define (df-set-contract! df series contractfn)
  (define col (df-get-series df series))
  (series-set-contract! col contractfn))

;; Count the number of NA (not available) values in SERIES.
(define (df-count-na df series)
  (define col (df-get-series df series))
  (series-na-count col))

;; Return true if SERIES has any NA (not available) values.  This is
;; equivalent to (> (df-count-na DF SERIES) 0), but it is faster.
(define (df-has-na? df series)
  (define col (df-get-series df series))
  (series-has-na? col))

;; Return true if SERIES has any values except NA (not available) values.
;; This is equivalent to (< (df-count-na DF SERIES) (df-row-count DF)), but
;; faster.
(define (df-has-non-na? df series)
  (define col (df-get-series df series))
  (series-has-non-na? col))

;; Return #t if VAL is the NA (not available) value for SERIES.
(define (df-is-na? df series val)
  (define col (df-get-series df series))
  (series-is-na? col val))


;;........................................................... Properties ....

;; Return the property names in the data frame DF, as a list of symbols.  The
;; names are returned in an unspecified order.
(define (df-property-names df)
  (hash-keys (data-frame-properties df)))

;; Put the property (KEY, VALUE) inside the data frame DF, possibly replacing
;; an existing one for KEY.
(define (df-put-property! df key value)
  (hash-set! (data-frame-properties df) key value))

;; Return the value for property KEY inside the data frame DF.  If there is no
;; property named KEY, the DEFAULT-VALUE-FN is invoked to produce a value (the
;; default function just returns #f)
(define (df-get-property df key [default-value-fn (lambda () #f)])
  (hash-ref (data-frame-properties df) key default-value-fn))

;; Delete the property KEY in the data frame DF.
(define (df-del-property! df key)
  (hash-remove! (data-frame-properties df) key))


;;............................................................. Indexing ....

;; Add the single-column index NAME to the data frame.  A single SERIES is
;; indexed using the LT function (a strict less-than function).  The
;; NA-IN-FRONT? variable determines whether the series NA values are placed in
;; front or at the back of the other rows.
(define (df-add-index! df name series lt #:na-in-front? (na-in-front? #f))
  (define index (make-sx df name (list series) (list lt) #:na-in-front? na-in-front?))
  (hash-set! (data-frame-secondary-indexes df) name index))

;; Add a multi-column index NAME to the data frame.  The list of SERIES is
;; indexed using a list of LT-FUNCTIONS (one for each series).  The
;; NA-IN-FRONT? variable determines whether the series NA values are placed in
;; front or at the back of the other rows.
(define (df-add-index*! df name series lt-functions #:na-in-front? (na-in-front? #f))
  (define index (make-sx df name series lt-functions #:na-in-front? na-in-front?))
  (hash-set! (data-frame-secondary-indexes df) name index))

;; Delete the index NAME from the data frame -- it is not an error to delete
;; an index that does not exist.
(define (df-del-index! df name)
  (hash-remove! (data-frame-secondary-indexes df) name))

;; Find the index NAME in the data series and return it.  If the index was
;; invalidated, it is rebuilt before returning it.  An error is reported if
;; the index does not exist.
(define (df-get-index df name)
  (define sx
    (hash-ref (data-frame-secondary-indexes df) name
              (lambda ()
                (df-raise "df-get-index (\"~a\"): no such index" name))))
  (sx-maybe-rebuild! df sx)
  sx)

;; Return the list of index names present in the data frame DF -- the names
;; are not returned in any particular order.
(define (df-index-names df)
  (hash-keys (data-frame-secondary-indexes df)))

;; Return the list of series for the index INDEX-NAME -- the series are
;; returned in the order they are indexed
(define (df-index-series df index-name)
  (define sx (df-get-index df index-name))
  (sx-series sx))

;; Find an index which can be used for lookups on SERIES.  This is an index
;; whose first indexed column is SERIES (multi-column indexes can be used to
;; lookup partial keys).  Returns #f if no suitable index is found.
(define (find-suitable-index df series)
  (for/first ([sx (in-hash-values (data-frame-secondary-indexes df))]
              #:when (equal? series (car (sx-series sx))))
    sx))

;; Return the list of indices which use SERIES.  This function is used to
;; invalidate indices what are affected by a change to SERIES.
(define (indices-using-series df series)
  (for/list ([sx (in-hash-values (data-frame-secondary-indexes df))]
             #:when (member series (sx-series sx)))
    sx))

;; Remove all indices that use SERIES in any of their columns.
(define (drop-indices-using-series df series)
  (for/list ([sx (in-list (indices-using-series df series))])
    (df-del-index! df (sx-name sx))))

;; Invalidate all indices that use SERIES in any of their columns -- this is
;; used when SERIES has changed and the index needs to be rebuilt.
(define (invalidate-indices-using-series df series)
  (for/list ([sx (in-list (indices-using-series df series))])
    (sx-invalidate! sx)))


;;.......................................... Looking for Individual Rows ....

;; Return the position of VALUE in the data frame SERIES.  If SERIES is not
;; sorted, this will raise an error.  VALUE might not be present in the
;; series, in that case, the returned index is the position of the first
;; element which is greater than VALUE (i.e. the position where VALUE could be
;; inserted and still keep the series sorted).  A value of 0 is returned if
;; VALUE is less or equal than the first value of the series and a value of
;; (df-row-count df) is returned if the value is greater than all the values
;; in SERIES.
(define (df-index-of df series value #:exact-match? (exact-match? #f))
  (let ([s (df-get-series df series)])
    (if (series-is-sorted? s)
        (series-index-of s value #:exact-match? exact-match?)
        (let ([sx (find-suitable-index df series)])
          (unless sx
            (df-raise "df-index-of: ~a is not sorted and there is no index for it" series))
          (let ([pos (sx-index-of sx value)])
            (if exact-match?
                (and (< pos (series-size s))
                     (equal? (series-ref s pos) value)
                     pos)
                pos))))))

;; Return a list of indexes corresponding to each element in VALUES for
;; SERIES.  This is the same as calling `df-index-of` for each individual
;; value, but it is more efficient.
(define (df-index-of* df series #:exact-match? (exact-match? #f) . values)
  (let ([s (df-get-series df series)])
    (if (series-is-sorted? s)
        (for/list ([v (in-list values)])
          (series-index-of s v #:exact-match? exact-match?))
        (let ([sx (find-suitable-index df series)])
          (unless sx
            (df-raise "df-index-of*: ~a is not sorted and there is no index for it" series))
          (if exact-match?
              (let ((limit (series-size s)))
                (for/list ([v (in-list values)])
                  (define pos (sx-index-of sx v))
                  (and (< pos limit) (equal? (series-ref s pos) v) pos)))
              (for/list ([v (in-list values)])
                (sx-index-of sx v)))))))

;; Return a list of all position where VALUE is present in SERIES.  If the
;; series is sorted, the list is consecutive, but if an index is used, the
;; positions will not be.  Will raise an error either if SERIES is not sorted
;; or if there is no suitable index (i.e. this function will never do a full
;; scan of the series).
(define (df-all-indices-of df series value)
  (let ([s (df-get-series df series)])
    (if (series-is-sorted? s)
        (series-all-indices-of s value)
        (let ([sx (find-suitable-index df series)])
          (if sx
              (sx-all-indices-of sx value)
              (df-raise "df-all-indices-of: ~a is not sorted and there is no index for it" series))))))

;; Return the indices (position) of VALUE in the data frame DF in the given SERIES.
;; If SERIES is not sorted, this will raise an error. This will return a pair of the
;; leftmost and rightmost index in the given series, in case the value is duplicated.
;; All other behavior is similar to or the same as df-index-of.
(define (df-equal-range df series value)
  (let ([s (df-get-series df series)])
    (series-equal-range s value)))

;; Perform an indexed lookup: the index of VALUE is found in BASE-SERIES (a
;; series name) and the value at the same index is returned from SERIES, which
;; is either a single series or a list of series.  IF SERIES is a single
;; string, a single value is returned, if it is a list of names, a list of
;; values is returned.
;;
;; This is a combination of `df-index-of` and `df-ref`, but more efficient.
(define (df-lookup df base-series series value #:exact-match? (exact-match? #f))
  (let ((index (df-index-of df base-series value #:exact-match? exact-match?)))
    (if index
        (if (< index (df-row-count df))
            (if (list? series)
                (apply df-ref* df index series)
                (df-ref df index series))
            (if (list? series)
            (for/vector ([s (in-list series)])
              (df-na-value df s))
            (df-na-value df series)))
        #f)))

;; Perform an indexed lookup on multiple values.  Same as calling `df-lookup`
;; on each value in VALUES and returning the result as a list.
(define (df-lookup* df base-series series #:exact-match? (exact-match? #f) . values)
  (define slist
    (if (list? series)
        (for/list ([s series]) (df-get-series df s))
        (df-get-series df series)))
  (define limit (df-row-count df))
  (define nitems (if (list? series) (length series) 1))
  (for/list ([index (keyword-apply df-index-of* null null df base-series #:exact-match? exact-match? values)])
    (if index
        (if (< index limit)
            (if (list? slist)
                (for/vector #:length nitems ([s (in-list slist)])
                  (series-ref s index))
                (series-ref slist index))
            (if (list? slist)
                (for/vector #:length nitems ([s (in-list slist)])
                  (series-na s))
                (series-na slist)))
        #f)))

;; Perform an interpolated lookup: same as `df-lookup`, but if VALUE is not
;; found exactly in BASE-SERIES, it's relative position is determined and it
;; is used to interpolate values from the corresponding SERIES.  An
;; interpolation function can be specified, if the default one is not
;; sufficient.  This function is called once for each resulting SERIES.
(define (df-lookup/interpolated
         df base-series series value
         #:interpolate (interpolate (lambda (t v1 v2)
                                      (+ (* (- 1 t) v1) (* t v2)))))
  (define index (df-index-of df base-series value))
  (define slist (if (pair? series) series (list series)))
  (cond ((<= index 0)
         (if (pair? series)
             (apply df-ref* df 0 series)
             (df-ref df 0 series)))
        ((>= index (df-row-count df))
         (if (pair? series)
             (apply df-ref* df (sub1 (df-row-count df)) series)
             (df-ref df (sub1 (df-row-count df)) series)))
        (#t
         (let* ((pval (df-ref df (sub1 index) base-series))
                (aval (df-ref df index base-series))
                (t (/ (- value pval) (- aval pval)))
                (prev (apply df-ref* df (sub1 index) slist))
                (next (apply df-ref* df index slist))
                (result (for/vector #:length (length slist)
                                    ([p (in-vector prev)]
                                     [n (in-vector next)])
                          (interpolate t p n))))
           (if (list? series) result (vector-ref result 0))))))


;;....................................................... Selecting Data ....

;; Helper function to select only entries with valie values.  Usefull as a
;; parameter for the #:filter parameter of the select and select* methods of
;; a data-frame
(define (valid-only vec)
  (cond ((vector? vec)
         (and (for/and ([v (in-vector vec)]) v) #t))
        ((list? vec)
         (and (for/and ([v (in-list vec)]) v) #t))
        (#t
         (not (not vec)))))

;; Helper function used by the `in-data-frame...` functions to create
;; generators that iterate over data frames.  All our iterations are done
;; between two positions START and STOP and POS->ELEMENT is a function which
;; returns the values for a specified position.
(define (make-in-data-frame-sequence pos->element start stop)
  (make-do-sequence
   (lambda ()
     (values
      ;; pos->element
      pos->element
      ;; early-next-pos
      #f
      ;; next-pos
      (if (<= start stop) add1 sub1)
      ;; initial position
      start
      ;; continue-with-pos?
      (if (<= start stop)
          (lambda (pos) (< pos stop))
          (lambda (pos) (> pos stop)))
      ;; continue-with-val?
      #f
      ;; continue-with-pos+val?
      #f))))

;; Return a generator that produces values from a list of SERIES between START
;; and STOP rows.  The sequence produces values, each one corresponding to the
;; series names.  NOTE: this is intended to be used in `for` and related
;; constructs to iterate over elements in the data frame.
;;
;; Example:
;;
;; (for (([lat lon] (in-data-frame df "lat" "lon")))
;;    (printf "lat = ~a, lon = ~a~%" lat lon))
;;
(define (in-data-frame df
                       #:start (start 0)
                       #:stop (stop (df-row-count df))
                       . series)
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define vals (for/list ([s (in-list the-series)])
                    (series-ref s pos)))
     (apply values vals))
   start stop))

;; Same as `in-data-frame` but returns one single value, a list of all the
;; selected elements from the specified series.
(define (in-data-frame/as-list df
                       #:start (start 0)
                       #:stop (stop (df-row-count df))
                       . series)
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (for/list ([s (in-list the-series)])
       (series-ref s pos)))
   start stop))

;; Same as `in-data-frame` but returns one single value, a vector of all the
;; selected elements from the specified series.
(define (in-data-frame/as-vector
         df
         #:start (start 0)
         #:stop (stop (df-row-count df))
         . series)
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (define result-length (length series))
  (make-in-data-frame-sequence
   (lambda (pos)
     (for/vector #:length result-length
         ([s (in-list the-series)])
       (series-ref s pos)))
   start stop))


;; Sentinel used for in-data-frame/by-index and related functions.  This is
;; just an unique value so the user can actually pass #f as a valid value for
;; the index keys.
(define by-index-sentinel (vector 'by-index-sentinel))

;; Same as `in-data-frame`, but iteration is done over the index INDEX-NAME
;; between the FROM and TO values.  This function is intended to be used with
;; single-column keys, i.e. FROM and TO are single values, but multi-column
;; indexes can still be used for the iteration if the first indexed column is
;; the one iterated over between the FROM and TO values.
;;
;; If FROM is omitted, iteration starts from the first value in the index
;; order, if TO is omitted, iteration stops at the last value in the index
;; order.
;;
;; HINT: you can iterate over the elements with the same value by specifying
;; that value to both FROM and TO.
;;
;; The generator produces several values, one for each value in SERIES.
(define (in-data-frame/by-index df
                                #:index index-name
                                #:from [from by-index-sentinel]
                                #:to [to by-index-sentinel]
                                . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx (list from))))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx (list to))))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (define vals (for/list ([s (in-list the-series)])
                    (series-ref s index)))
     (apply values vals))
   start stop))

;; Same as `in-data-frame/by-index` but FROM and TO are multi value lists and
;; can be used to iterate over multi-column indexes.  You can iterate over
;; fewer columns that the index has.
(define (in-data-frame/by-index* df
                                 #:index index-name
                                 #:from [from by-index-sentinel]
                                 #:to [to by-index-sentinel]
                                . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx from)))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx to)))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (define vals (for/list ([s (in-list the-series)])
                    (series-ref s index)))
     (apply values vals))
   start stop))

;; Same as `in-data-frame/by-index` but produces single values, a list of
;; elements from the specified SERIES
(define (in-data-frame/by-index/as-list
         df
         #:index index-name
         #:from [from by-index-sentinel]
         #:to [to by-index-sentinel]
         . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx (list from))))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx (list to))))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (for/list ([s (in-list the-series)])
       (series-ref s index)))
   start stop))

;; Same as `in-data-frame/by-index` but produces single values, a vector of
;; elements from the specified SERIES
(define (in-data-frame/by-index/as-vector
         df
         #:index index-name
         #:from [from by-index-sentinel]
         #:to [to by-index-sentinel]
         . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx (list from))))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx (list to))))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (define result-length (length series))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (for/vector #:length result-length
         ([s (in-list the-series)])
       (series-ref s index)))
   start stop))

;; Same as `in-data-frame/by-index*` but produces single values, a list of
;; elements from the specified SERIES
(define (in-data-frame/by-index*/as-list
         df
         #:index index-name
         #:from [from by-index-sentinel]
         #:to [to by-index-sentinel]
         . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx from)))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx to)))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (for/list ([s (in-list the-series)])
       (series-ref s index)))
   start stop))

;; Same as `in-data-frame/by-index*` but produces single values, a vector of
;; elements from the specified SERIES
(define (in-data-frame/by-index*/as-vector
         df
         #:index index-name
         #:from [from by-index-sentinel]
         #:to [to by-index-sentinel]
         . series)
  (define sx (df-get-index df index-name))
  (define start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx from)))
  (define stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx to)))
  (define the-series (for/list ([sn (in-list series)])
                       (df-get-series df sn)))
  (define result-length (length series))
  (make-in-data-frame-sequence
   (lambda (pos)
     (define index (sx-index-ref sx pos))
     (for/vector #:length result-length
         ([s (in-list the-series)])
       (series-ref s index)))
   start stop))

;; Returns a vector with the values in the series NAME from the data frame DF.
;; START and STOP indicate the first and one-before-last row to be
;; selected. FILTER-FN, when present, will filter values selected, only values
;; for which FILTER-FN returns #t will be added to the resulting vector.
;;
;; If there is no FILTER-FN specified, the resulting vector will have (- STOP
;; START) elements.  If there is a filter, the number of elements depends on
;; how many are filtered out by this function.
(define (df-select df name
                   #:filter (filter-fn #f)
                   #:start (start 0)
                   #:stop (stop (df-row-count df)))
  (define col (df-get-series df name))
  (if filter-fn
      (for/vector ([d (in-series col start stop)] #:when (filter-fn d)) d)
      (for/vector #:length (- stop start) ([d (in-series col start stop)]) d)))

;; Same as `df-select` but selects in the order defined by INDEX-NAME between
;; FROM and TO keys (which are single column values)
(define (df-select/by-index df name
                           #:filter (filter-fn #f)
                           #:index index-name
                           #:from (from by-index-sentinel)
                           #:to (to by-index-sentinel))
  (df-select/by-index*
   df name
   #:filter filter-fn
   #:index index-name
   #:from (if (eq? from by-index-sentinel) by-index-sentinel (list from))
   #:to (if (eq? to by-index-sentinel) by-index-sentinel (list to))))

;; Same as `df-select` but selects in the order defined by INDEX-NAME between
;; FROM and TO keys (which are multi-column values)
(define (df-select/by-index* df name
                             #:filter (filter-fn #f)
                             #:index index-name
                             #:from (from by-index-sentinel)
                             #:to (to by-index-sentinel))
  (if filter-fn
      (for/vector ([d (in-data-frame/by-index
                       df #:index index-name #:from from #:to to)]
                   #:when (filter-fn d))
        d)
      ;; If there is no filter function specified, we know how many elements
      ;; we will fetch, which means we can avoid some memory allocations in
      ;; for/vector
      (let* ([sx (df-get-index df index-name)]
             [s (df-get-series df name)]
             [start (if (eq? from by-index-sentinel)
                        0
                        (sx-lower-bound sx from))]
             [stop (if (eq? to by-index-sentinel)
                       (df-row-count df)
                       (sx-upper-bound sx to))])
        (for/vector #:length (- stop start)
            ([d (make-in-data-frame-sequence
                 (lambda (pos)
                   (define index (sx-index-ref sx pos))
                   (series-ref s index))
                 start stop)])
          d))))

;; Return a vector where each element is a vector containing values from
;; multiple SERIES between the START and STOP rows of the data frame.
;; FILTER-FN, when present, will filter values selected, only values for which
;; FILTER-FN returns #t will be added to the resulting vector.
;;
;; If there is no FILTER-FN specified, the resulting vector will have (- STOP
;; START) elements.  If there is a filter, the number of elements depends on
;; how many are filtered out by this function.
(define (df-select* df
                    #:filter (filter-fn #f)
                    #:start (start 0)
                    #:stop (stop (df-row-count df)) . series)
  (define generator
    (keyword-apply in-data-frame/as-vector null null df series #:start start #:stop stop))
  (if filter-fn
      (for*/vector ([item generator]
                    [val (in-value item)]
                    #:when (filter-fn val))
        val)
      (for/vector #:length (- stop start) ([item generator]) item)))

;; Combine `df-select*` and `df-select/by-index`
(define (df-select*/by-index  df
                              #:filter (filter-fn #f)
                              #:index index-name
                              #:from (from by-index-sentinel)
                              #:to (to by-index-sentinel)
                              . series)
  (keyword-apply
   df-select*/by-index*
   null null
   df series
   #:filter filter-fn
   #:index index-name
   #:from (if (eq? from by-index-sentinel) by-index-sentinel (list from))
   #:to (if (eq? to by-index-sentinel) by-index-sentinel (list to))))

;; Combine `df-select*` and `df-select/by-index*`
(define (df-select*/by-index* df
                              #:filter (filter-fn #f)
                              #:index index-name
                              #:from (from by-index-sentinel)
                              #:to (to by-index-sentinel)
                              . series)
  (if filter-fn
      (let ([generator (keyword-apply
                        in-data-frame/by-index
                        null null
                        df series #:index index-name #:from from #:to to)])
        (for/vector ([d generator] #:when (filter-fn d)) d))
      ;; If there is no filter function specified, we know how many elements
      ;; we will fetch, which means we can avoid some memory allocations in
      ;; for/vector
      (let* ([sx (df-get-index df index-name)]
             [the-series (for/list ([n (in-list series)])
                           (df-get-series df n))]
             [start (if (eq? from by-index-sentinel)
                        0
                        (sx-lower-bound sx from))]
             [stop (if (eq? to by-index-sentinel)
                       (df-row-count df)
                       (sx-upper-bound sx to))]
             [series-count (length series)])
        (for/vector #:length (- stop start)
            ([d (make-in-data-frame-sequence
                 (lambda (pos)
                   (define index (sx-index-ref sx pos))
                   (for/vector #:length series-count
                       ([s (in-list the-series)])
                     (series-ref s index)))
                 start stop)])
          d))))

;; Map the function FN over a list of SERIES between START and STOP.  Returns
;; a vector with the values that FN returns.  FN is a function of ether one or
;; two arguments.  If FN is a function with one argument, it is called with
;; the values from all SERIES as a single vector.  If FN is a function of two
;; arguments, it is called with the current and previous set of values, as
;; vectors (this allows calculating "delta" values).  I.e. FN is invoked as
;; (FN PREV CURRENT).  If FN accepts two arguments, it will be invoked as (FN
;; #f CURRENT) for the first element of the iteration.
(define (df-map df series fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define generator (keyword-apply in-data-frame/as-list null null df
                                   (if (string? series) (list series) series)
                                   #:start start #:stop stop))
  (df-map/internal generator fn (- stop start)))

;; Same as `df-map` but maps in the order defined by INDEX-NAME between FROM
;; and TO keys (which are single column values)
(define (df-map/by-index df series fn
                          #:index index-name
                          #:from (from by-index-sentinel)
                          #:to (to by-index-sentinel))
  (df-map/by-index*
   df series fn
   #:index index-name
   #:from (if (eq? from by-index-sentinel) by-index-sentinel (list from))
   #:to (if (eq? to by-index-sentinel) by-index-sentinel (list to))))

;; Same as `df-map` but maps in the order defined by INDEX-NAME between FROM
;; and TO keys (which are multi-column values)
(define (df-map/by-index* df series fn
                          #:index index-name
                          #:from (from by-index-sentinel)
                          #:to (to by-index-sentinel))
  (let* ([sx (df-get-index df index-name)]
         [the-series (for/list ([n (in-list series)])
                       (df-get-series df n))]
         [start (if (eq? from by-index-sentinel)
                    0
                    (sx-lower-bound sx from))]
         [stop (if (eq? to by-index-sentinel)
                   (df-row-count df)
                   (sx-upper-bound sx to))])
    (define generator
      (make-in-data-frame-sequence
       (lambda (pos)
         (define index (sx-index-ref sx pos))
         (for/list ([s (in-list the-series)])
           (series-ref s index)))
       start stop))
    (df-map/internal generator fn (- stop start))))

;; Internal implementation for mapping over series, used by all the above
;; functions.
(define (df-map/internal generator fn result-length)
  (define need-prev-val? (eq? (procedure-arity fn) 2))
  (if need-prev-val?
      (let ([prev-val #f])
        (for/vector #:length result-length ([val generator])
          (begin0 (fn prev-val val)
            (set! prev-val val))))
      (for/vector #:length result-length ([val generator])
        (fn val))))

;; Iterate the function FN over a list of SERIES between START and STOP,
;; discarding the results of FN.  FN is invoked as for `df-map`, which see.
(define (df-for-each df series fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define generator (keyword-apply in-data-frame/as-list null null df
                                   (if (string? series) (list series) series)
                                   #:start start #:stop stop))
  (df-for-each/internal generator fn))

;; Same as `df-for-each` but iterates in the order defined by INDEX-NAME
;; between FROM and TO keys (which are single column values)
(define (df-for-each/by-index df series fn
                              #:index index-name
                              #:from (from by-index-sentinel)
                              #:to (to by-index-sentinel))
  (df-for-each/by-index*
   df series fn
   #:index index-name
   #:from (if (eq? from by-index-sentinel) by-index-sentinel (list from))
   #:to (if (eq? to by-index-sentinel) by-index-sentinel (list to))))

;; Same as `df-for-each` but iterates in the order defined by INDEX-NAME
;; between FROM and TO keys (which are multi column values)
(define (df-for-each/by-index* df series fn
                               #:index index-name
                               #:from (from by-index-sentinel)
                               #:to (to by-index-sentinel))
  (define generator
    (keyword-apply
     in-data-frame/by-index*/as-list
     null null
     df #:index index-name #:from from #:to to
     series))
  (df-for-each/internal generator fn))

;; Internal implementation for iterating over the series, used by all the
;; "for-each" implementation above.
(define (df-for-each/internal generator fn)
  (define need-prev-val? (eq? (procedure-arity fn) 2))
  (if need-prev-val?
      (let ([prev-val #f])
        (for ([val generator])
          (fn prev-val val)
          (set! prev-val val)))
      (for ([val generator])
        (fn val))))

;; Fold a function FN over a list of SERIES between START and STOP.  INIT-VAL
;; is the initial value for the fold operation.  The last value returned by FN
;; is returned by this function.  FN is a function of ether two or three
;; arguments.  If FN is a function with two arguments, it is called with the
;; fold value plus the values from all SERIES is passed in as a single vector.
;; If FN is a function of three arguments, it is called with the fold value
;; plus the current and previous set of values, as vectors (this allows
;; calculating "delta" values).  I.e. FN is invoked as (FN VAL PREV CURRENT).
;; If FN accepts two arguments, it will be invoked as (FN INIT-VAL #f CURRENT)
;; for the first element of the iteration.
(define (df-fold df series init-val fn #:start (start 0) #:stop (stop (df-row-count df)))
  (define generator
    (keyword-apply in-data-frame/as-list null null df
                   (if (string? series) (list series) series)
                   #:start start #:stop stop))
  (df-fold/internal generator fn init-val))

;; Same as `df-fold` but folds in the order defined by INDEX-NAME between FROM
;; and TO keys (which are single column values)
(define (df-fold/by-index df series init-val fn
                          #:index index-name
                          #:from (from by-index-sentinel)
                          #:to (to by-index-sentinel))
  (df-fold/by-index*
   df series init-val fn
   #:index index-name
   #:from (if (eq? from by-index-sentinel) by-index-sentinel (list from))
   #:to (if (eq? to by-index-sentinel) by-index-sentinel (list to))))

;; Same as `df-fold` but folds in the order defined by INDEX-NAME between FROM
;; and TO keys (which are multi column values)
(define (df-fold/by-index* df series init-val fn
                           #:index index-name
                           #:from (from by-index-sentinel)
                           #:to (to by-index-sentinel))
  (define generator
    (keyword-apply
     in-data-frame/by-index*/as-list
     null null
     df #:index index-name #:from from #:to to
     series))
  (df-fold/internal generator fn init-val))

;; Internal implementation of the folding, used by the implementations above.
(define (df-fold/internal generator fn init-val)
  (define need-prev-val? (eq? (procedure-arity fn) 3))
  (if need-prev-val?
      (for/fold ([accumulator init-val]
                 [prev-val #f]
                 #:result accumulator)
                ([val generator])
        (values (fn accumulator prev-val val) val))
      (for/fold ([accumulator init-val])
                ([val generator])
        (fn accumulator val))))


;;.................................................... Secondary Indexes ....

;; A secondary index, defining a different iteration order for rows in a data
;; frame.  A data frame can have multiple secondary indexes defined, thus
;; multiple iteration orders.  Also, secondary indexes can be defined over
;; multiple columns allowing a "sort by X, then by Y" construct.
(struct sx
  (name                     ; the name of the index
   series                   ; list of series names indexed (a list of strings)
   less-than-fn ; a "less than" function for the index -- see MAKE-SX-COMPARATOR
   [data #:mutable])        ; the indexed data
  #:transparent)

;; Make a function which can compare two values in an index using a strict
;; "less than" relationship.  The function takes a list of basic comparators,
;; one for each column as well as a list of NA-values (meaning "not
;; available").  When NA-ON-FRONT? is #t, NA's compare such that they are
;; placed before all other values, otherwise they compare such that they are
;; placed at the end.
(define (make-sx-comparator less-than-functions na-values na-on-front?)
  (lambda (a b)
    ;; The A and B keys we compare have the position as the first element, so
    ;; the comparison skips the first element of A and B.
    (let loop ([a (cdr a)]
               [b (cdr b)]
               [less-than-functions less-than-functions]
               [na-values na-values])
      (if (or (null? a) (null? b))
          #f
          (let ([va (car a)]
                [vb (car b)]
                [less-than? (car less-than-functions)]
                [na (car na-values)])
            (cond
              ;; If both VA and VB are #f, continue the comparison
              ((and (equal? va na) (equal? vb na))
               (loop (cdr a) (cdr b)
                          (cdr less-than-functions)
                          (cdr na-values)))
              ((equal? va na) na-on-front?)
              ((equal? vb na) (not na-on-front?))
              ((less-than? va vb) #t)
              ((less-than? vb va) #f)
              (else (loop (cdr a) (cdr b)
                          (cdr less-than-functions)
                          (cdr na-values)))))))))

;; Create the index NAME, indexing SERIES. LESS-THAN-FUNCTIONS and
;; NA-IN-FRONT? are passed to the MAKE-SX-COMPARATOR. The index is returned,
;; but not added to the data frame DF.
(define (make-sx df name series less-than-functions #:na-in-front? (na-in-front? #f))
  (define na-values
    (for/list ([name (in-list series)])
      (df-na-value df name)))
  (define less-than-fn
    (make-sx-comparator less-than-functions na-values na-in-front?))
  (define data
    (for/vector #:length (df-row-count df)
        ([key (apply in-data-frame/as-list df series)]
         [pos (in-naturals)])
      (cons pos key)))
  (vector-sort! data less-than-fn)
  (sx name series less-than-fn data))

;; Invalidate the index SX -- this is done when the underlying data in the
;; series has changed.  Note that we currently invalidate and rebuild the
;; entire index, which is not ideal, but data-frames are meant to stay
;; unchanged once constructed...
(define (sx-invalidate! sx)
  (set-sx-data! sx #f))

;; Rebuild an invalidated index
(define (sx-maybe-rebuild! df sx)
  (unless (sx-data sx)
    (define data
      (for/vector #:length (df-row-count df)
          ([key (apply in-data-frame/as-list df (sx-series sx))]
           [pos (in-naturals)])
        (cons pos key)))
    (vector-sort! data (sx-less-than-fn sx))
    (set-sx-data! sx data)))

;; Find the position in the index of the first value VALS
(define (sx-lower-bound sx vals)
  (define key (cons #f vals))
  (lower-bound/no-checks (sx-data sx) key #:cmp (sx-less-than-fn sx)))

;; Find the position in the index of the last value VALS
(define (sx-upper-bound sx vals)
  (define key (cons #f vals))
  (upper-bound/no-checks (sx-data sx) key #:cmp (sx-less-than-fn sx)))

;; Find the indexed position for the value in the index POS.
(define (sx-index-ref sx pos)
  (car (vector-ref (sx-data sx) pos)))

;; Find the indexed position the KEY created by VALS in the index.
(define (sx-index-of sx . vals)
  (define key (cons #f vals))
  (define data (sx-data sx))
  (define sindex (lower-bound data key #:cmp (sx-less-than-fn sx)))
  (if (>= sindex (vector-length data))
      (vector-length data)
      (car (vector-ref data sindex))))

;; Return a list of all positions where VAL occurs in the data frame, using
;; this index.
(define (sx-all-indices-of sx val)
  (define key (list #f val))
  (define data (sx-data sx))
  (define-values (low high) (equal-range data key #:cmp (sx-less-than-fn sx)))
  (if (>= low (vector-length data))
      '()
      (for/list ([idx (in-range low high)])
        (car (vector-ref data idx)))))

;; Rename a series in the index INDEX from OLD-SERIES-NAME to NEW-SERIES-NAME
;; -- this creates a new SX struct with all the other data unchanged.
(define (sx-rename-series index old-series-name new-series-name)
  (define nseries (for/list ([s (in-list (sx-series index))])
                    (if (equal? s old-series-name) new-series-name s)))
  (struct-copy sx index [series nseries]))


;;............................................................. Provides ....

(define index/c (or/c #f -1 exact-nonnegative-integer?))
(define mapfn/c (or/c (-> any/c any/c) (-> any/c any/c any/c)))
(define foldfn/c (or/c (-> any/c any/c any/c) (-> any/c any/c any/c any/c)))
(define less-than-fn/c (-> any/c any/c boolean?))

(provide  index/c
          mapfn/c
          foldfn/c
          less-than-fn/c)

(provide/contract
 (make-data-frame (-> data-frame?))
 (df-series-names (-> data-frame? (listof string?)))
 (df-property-names (-> data-frame? (listof symbol?)))
 (df-contains? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-contains/any? (->* (data-frame?) () #:rest (listof string?) boolean?))
 (df-put-property! (-> data-frame? symbol? any/c any/c))
 (df-get-property (->* (data-frame? symbol?) (any/c) any/c))
 (df-del-property! (-> data-frame? symbol? any/c))
 (df-row-count (-> data-frame? exact-nonnegative-integer?))
 (df-is-sorted? (-> data-frame? string? boolean?))
 (df-all-indices-of (-> data-frame? string? any/c (listof exact-nonnegative-integer?)))
 (df-equal-range (-> data-frame? string? any/c (values index/c index/c)))
 (df-ref (-> data-frame? index/c string? any/c))
 (df-set! (-> data-frame? index/c any/c string? any/c))
 (df-ref* (->* (data-frame? index/c) #:rest (listof string?) vector?))
 (df-add-series! (-> data-frame? series? any/c))
 (df-del-series! (-> data-frame? string? any/c))
 (df-rename-series! (-> data-frame? string? string? any/c))
 (df-add-derived! (-> data-frame? string? (listof string?) mapfn/c any/c))
 (df-add-lazy! (-> data-frame? string? (listof string?) mapfn/c any/c))
 (df-set-sorted! (-> data-frame? string? (or/c #f (-> any/c any/c boolean?)) any/c))
 (df-set-contract! (-> data-frame? string? (or/c #f (-> any/c boolean?)) any/c))
 (df-count-na (-> data-frame? string? exact-nonnegative-integer?))
 (df-shallow-copy (-> data-frame? data-frame?))
 (df-duplicate-series (-> data-frame? string? series?))
 (df-na-value (-> data-frame? string? any/c))
 (df-is-na? (-> data-frame? string? any/c boolean?))
 (df-has-na? (-> data-frame? string? boolean?))
 (df-has-non-na? (-> data-frame? string? boolean?))
 (data-frame? (-> any/c boolean?))
 (valid-only (-> any/c boolean?))

 (df-add-index! (->* (data-frame? string? string? less-than-fn/c)
                     (#:na-in-front? boolean?)
                     any/c))
 ;; NOTE: we can extend the contract so that the number of series matches the
 ;; number of "less-than" functions.
 (df-add-index*! (->* (data-frame? string?
                                   (listof string?)
                                   (listof less-than-fn/c))
                      (#:na-in-front? boolean?)
                      any/c))
 (df-del-index! (-> data-frame? string? any/c))
 (df-index-names (-> data-frame? (listof string?)))
 (df-index-series (-> data-frame? string? (listof string?)))

 (df-index-of (->* (data-frame? string? any/c)
                   (#:exact-match? boolean?)
                   index/c))
 (df-index-of* (->* (data-frame? string?)
                    (#:exact-match? boolean?)
                    #:rest list?
                    (listof index/c)))

 (df-lookup (->* (data-frame? string? (or/c string? (listof string?)) any/c)
                 (#:exact-match? boolean?)
                 any/c))
 (df-lookup* (->* (data-frame? string? (or/c string? (listof string?)))
                  (#:exact-match? boolean?)
                  #:rest list? list?))
 (df-lookup/interpolated (->* (data-frame? string? (or/c string? (listof string?)) any/c)
                              (#:interpolate (-> real? any/c any/c any/c))
                              any/c))


 (df-select
  (->* (data-frame? string?)
       (#:filter (or/c #f (-> any/c any/c))
        #:start index/c #:stop index/c)
       vector?))
 (df-select/by-index
  (->* (data-frame? string? #:index string?)
       (#:filter (or/c #f (-> any/c any/c))
        #:from any/c #:to any/c)
       vector?))
 (df-select/by-index*
  (->* (data-frame? string? #:index string?)
       (#:filter (or/c #f (-> any/c any/c))
        #:from (listof any/c) #:to (listof any/c))
       vector?))
 (df-select*
  (->* (data-frame?) (#:filter (or/c #f (-> any/c any/c))
                      #:start index/c #:stop index/c)
       #:rest (listof string?) vector?))
 (df-select*/by-index
  (->* (data-frame? #:index string?)
       (#:filter (or/c #f (-> any/c any/c))
        #:from any/c #:to any/c)
       #:rest (listof string?)
       vector?))
 (df-select*/by-index*
  (->* (data-frame? #:index string?)
       (#:filter (or/c #f (-> any/c any/c))
        #:from (listof any/c) #:to (listof any/c))
       #:rest (listof string?)
       vector?))

 (df-map
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c)
       (#:start index/c #:stop index/c)
       vector?))
 (df-map/by-index
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c #:index string?)
       (#:from any/c #:to any/c)
       vector?))
 (df-map/by-index*
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c #:index string?)
       (#:from (listof any/c) #:to (listof any/c))
       vector?))

 (df-for-each
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c)
       (#:start index/c #:stop index/c)
       any/c))
 (df-for-each/by-index
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c #:index string?)
       (#:from any/c #:to any/c)
       any/c))
 (df-for-each/by-index*
  (->* (data-frame? (or/c string? (listof string?)) mapfn/c #:index string?)
       (#:from (listof any/c) #:to (listof any/c))
       any/c))

 (df-fold
  (->* (data-frame? (or/c string? (listof string?)) any/c foldfn/c)
       (#:start index/c #:stop index/c)
       any/c))
 (df-fold/by-index
  (->* (data-frame? (or/c string? (listof string?)) any/c foldfn/c #:index string?)
       (#:from any/c #:to any/c)
       any/c))
 (df-fold/by-index*
  (->* (data-frame? (or/c string? (listof string?)) any/c foldfn/c #:index string?)
       (#:from (listof any/c) #:to (listof any/c))
       any/c))

 (in-data-frame
  (->* (data-frame?)
       (#:start index/c #:stop index/c)
       #:rest (listof string?)
       sequence?))
 (in-data-frame/as-vector
  (->* (data-frame?)
       (#:start index/c #:stop index/c)
       #:rest (listof string?)
       sequence?))
 (in-data-frame/as-list
  (->* (data-frame?)
       (#:start index/c #:stop index/c)
       #:rest (listof string?)
       sequence?))

 (in-data-frame/by-index
  (->* (data-frame? #:index string?)
       (#:from any/c #:to any/c)
       #:rest (listof string?)
       sequence?))
 (in-data-frame/by-index/as-vector
  (->*  (data-frame? #:index string?)
        (#:from any/c #:to any/c)
        #:rest (listof string?)
        sequence?))
 (in-data-frame/by-index/as-list
  (->*  (data-frame? #:index string?)
        (#:from any/c #:to any/c)
        #:rest (listof string?)
        sequence?))

 (in-data-frame/by-index*
  (->*  (data-frame? #:index string?)
        (#:from (listof any/c) #:to (listof any/c))
         #:rest (listof string?)
         sequence?))

 (in-data-frame/by-index*/as-vector
  (->*  (data-frame? #:index string?)
        (#:from (listof any/c) #:to (listof any/c))
         #:rest (listof string?)
         sequence?))

 (in-data-frame/by-index*/as-list
  (->*  (data-frame? #:index string?)
        (#:from (listof any/c) #:to (listof any/c))
         #:rest (listof string?)
         sequence?))

 )

;; These functions are provided so we can write tests for them
(provide
 make-sx-comparator)
