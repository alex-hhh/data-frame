#lang racket/base
;; for-df.rkt -- macros for row-by-row data-frame construction
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2021 Hazel Levine <hazel@knightsofthelambdacalcul.us>
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
(require (for-syntax racket/base
                     syntax/for-body
                     syntax/parse)
         "df.rkt"
         "series.rkt")

;;;; helpers for exponential backoff
;; Creates a mutable vector with twice the length of the given input vector
;; `vec`, and the same contents up until the end of the input vector. The new
;; contents are initialized to zeroes.
(define (grow-vector vec)
  (define n (vector-length vec))
  (define new-vec (make-vector (* 2 n)))
  (vector-copy! new-vec 0 vec 0 n)
  new-vec)

;; Truncates the input vector `vec` to the given length `i`, while retaining
;; its elements up to that point.
(define (shrink-vector vec i)
  (define new-vec (make-vector i))
  (vector-copy! new-vec 0 vec 0 i)
  new-vec)

;;;; for_/data-frame
;; Creates a data-frame with the given column names `col-names` and data `vecs`.
(define (make-df-with-series col-names vecs)
  (define new-df (make-data-frame))
  (for ([name (in-list col-names)]
        [vec (in-list vecs)])
    (df-add-series! new-df (make-series name #:data vec)))
  new-df)

;; Constructs a for form with the given syntax `stx` and for/fold/derived form
;; `for_/fold/derived-stx`, with the given series names `(column:id ...)`.
(define-for-syntax (for_/data-frame stx for_/fold/derived-stx)
  (syntax-parse stx
    [(_ (column:id ...) clauses body ... tail-expr)
     #:with orig-stx stx
     #:with for_/fold/derived for_/fold/derived-stx
     #:with ((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... tail-expr))
     ; each series name is specified in (column:id ...), convert them to strings
     #'(let ([columns (list (symbol->string 'column) ...)])
         (call-with-values
          ; generate each vector to be used as a series
          (λ ()
            (for_/fold/derived orig-stx
              ; make an accumulator for the index into each vector,
              ; and an accumulator for each new series to create, with default length 16
              ([i 0]
               [column (make-vector 16)] ...)
              clauses pre-body ...

              ; increment the index accumulator, and for each column accumulator, determine
              ; if we have reached the end. if we have, call `grow-vector` to increase its
              ; length. then, mutably update the vector, and continue looping
              (apply values
                     (add1 i)
                     (for/list ([col-vec (in-list (list column ...))]
                                [value (in-list (call-with-values (λ () post-body ...) list))])
                       (define new-vec (if (eq? i (vector-length col-vec))
                                           (grow-vector col-vec)
                                           col-vec))
                       (vector-set! new-vec i value)
                       new-vec))))
          ; take all the values from the iteration, and then shrink each vector accumulator to
          ; the final index we had before we stopped iterating. then, create a new data-frame
          ; with the given column names and vector accumulators
          (λ vecs
            (define len (car vecs))
            (define to-add (cdr vecs))
            (make-df-with-series columns
                                 (map (λ (v) (shrink-vector v len))
                                      to-add)))))]))

;;;; for/data-frame and for*/data-frame
;; Constructs a data-frame with the given columns, row-by-row. Each input
;; sequence is bound in parallel.
(define-syntax (for/data-frame stx)
  (for_/data-frame stx #'for/fold/derived))

;; Constructs a data-frame with the given columns, row-by-row, except that each
;; sequence is bound sequentially, not in parallel.
(define-syntax (for*/data-frame stx)
  (for_/data-frame stx #'for*/fold/derived))

;;;; provides
(provide for/data-frame for*/data-frame)
