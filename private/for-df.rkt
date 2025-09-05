#lang racket/base
;; for-df.rkt -- macros for row-by-row data-frame construction
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2021 Hazel Levine <hazel@knightsofthelambdacalcul.us>
;; Copyright (c) 2025 Alex Harsányi <alexharsanyi@gmail.com>
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

;; Constructs a for form with the given syntax `stx` and for/fold/derived form
;; `for_/fold/derived-stx`, with the given series names `(column:id ...)`.
(define-for-syntax (for_/data-frame stx for_/fold/derived-stx)
  (syntax-parse stx
    [(name:id (column:id ...) #:initial-capacity capacity:expr clauses body ... tail-expr)
     #:with orig-stx stx
     #:with for_/fold/derived for_/fold/derived-stx
     #:with ((pre-body ...) (post-body ...)) (split-for-body stx #'(body ... tail-expr))
     ;; each series name is specified in (column:id ...), convert them to
     ;; strings
     #'(let* ([series (list (make-series (symbol->string 'column) #:capacity capacity) ...)]
              [series-count (length series)])
         (for_/fold/derived
          orig-stx
          ((_unused (void)))            ; TODO: how to get rid of this?
          clauses pre-body ...
          (call-with-values
           (lambda () post-body ...)
           (lambda vs
             (unless (= (length vs) series-count)
               (raise-result-error
                (syntax->datum #'name)
                (format "body produced wrong number of values: got ~a, expecting ~a"
                        (length vs) series-count)
                vs))
             ;; Avoid using for-each, as this would check again the length of
             ;; its inputs, which is a performance penalty
             (let loop ([series series]
                        [vs vs])
               (unless (null? series)
                 (unsafe-series-push-back! (car series) (car vs))
                 (loop (cdr series) (cdr vs)))))))
         (make-data-frame #:series series))]
    [(name:id (column:id ...) clauses body ... tail-expr)
     ;; Note: we use a large(ish) default initial capacity, making data frames
     ;; use more memory for toy projects when a small number of rows are kept
     ;; in a DF.  However, this reduces the number of re-allocations when DF's
     ;; with large number of rows are used, which is what the intended
     ;; purposes of the data frame object is.
     (for_/data-frame #'(name (column ...) #:initial-capacity 256 clauses body ... tail-expr) for_/fold/derived-stx)]
    ))

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
