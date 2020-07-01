#lang racket/base

;; csv.rkt -- read and write data frames to CVS files
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
         racket/format
         racket/list
         racket/string
         "df.rkt"
         "series.rkt")


;;............................................................ write-csv ....

;; Quote the string STR, as per CSV rules: the string is enclosed in quotes
;; and any quotes inside the string are doubled.
(define (quote-string str)
  (string-append "\"" (string-replace str "\"" "\"\"") "\""))

;; Write in CSV format the data frame DF to the output port OUTP.  If SERIES,
;; if non-null, denote the series to be written.  If null, all the series are
;; written out in an unspecified order.  Rows between START and STOP are
;; written out.
(define (write-csv df outp series #:start start #:stop stop)
  (define first? #t)
  (define columns (if (null? series) (df-series-names df) series))
  (for ([header (in-list columns)])
    (if first?
        (set! first? #f)
        (write-string "," outp))
    (write-string (quote-string header) outp))
  (newline outp)
  (df-for-each
   df
   columns
   (lambda (val)
     (define first? #t)
     (for ([col (in-list columns)]
           [item (in-list val)])
       (if first?
           (set! first? #f)
           (write-string "," outp))
       (define oitem
         (cond
           ((df-is-na? df col item) "") ; this is not very fast...
           ((string? item) (quote-string item))
           ((real? item)
            (~a
             (if (exact-integer? item)
                 item
                 (exact->inexact item))))
           ;; otherwise we write in a way that we might be able to read it
           ;; back... this would work for transparent structs...
           (#t (quote-string (~s item)))))
       (write-string oitem outp))
     (newline outp))
   #:start start #:stop stop))

;; Write the data frame DF to OUTP which is either an output port or a string,
;; in which case it is assumed to be a file name.  The series to be written
;; out can be specified as the SERIES list.  If SERIES is empty, all series
;; are written out as columns in an unspecified order.  START and STOP denote
;; the beginning and end rows to be written out, by default all rows are
;; written out.
(define (df-write/csv df outp #:start (start 0) #:stop (stop (df-row-count df)) . series)
  (if (path-string? outp)
      (call-with-output-file outp
        #:mode 'text #:exists 'truncate/replace
        (lambda (o)
          (write-csv df o series #:start start #:stop stop)))
      (write-csv df outp series #:start start #:stop stop)))



;;............................................................. read-csv ....

(define (->cell data maybe-number? contains-whitespace?)
  (define as-string (list->string (reverse data)))
  (if maybe-number?
      (let ([v (if contains-whitespace? (string-trim as-string) as-string)])
        (or (string->number v 10) v))
      as-string))

;; NOTE: returns a list of characters in reverse order
(define (slurp-string in)
  (let loop ((current '())
             (maybe-number? #t)
             (contains-whitespace? #f))
    (let ((c (read-char in)))
      (cond ((eof-object? c)
             (values current maybe-number? contains-whitespace?))
            ((equal? c #\newline)
             ;; Recognize #\newline + #\return combinations
             (when (equal? (peek-char in) #\return) (read-char in))
             (values current maybe-number? contains-whitespace?))
            ((equal? c #\return)
             ;; Recognize #\return + #\newline combinations
             (when (equal? (peek-char in) #\newline) (read-char in))
             (values current maybe-number? contains-whitespace?))
            ((equal? c #\")
             (if (equal? (peek-char in) #\")
                 (begin
                   (read-char in)             ; consume the next char
                   (loop (cons c current) maybe-number? contains-whitespace?))
                 (values current maybe-number? contains-whitespace?)))
            (#t
             (loop (cons c current)
                   (and maybe-number? (or (char-numeric? c) (char-punctuation? c)))
                   (or contains-whitespace? (char-whitespace? c))))))))
  
;; Parse a LINE from a CSV file and return the list of "cells" in it as
;; strings or numbers also returns the number of cells in the list.  Takes
;; special care that comma characters "," inside strings are correctly
;; handled.  Also double quotes inside strings are unquoted.
;;
;; NOTE: cells are produced in reverse order!
(define (parse-line in quoted-numbers?)
  (let loop ((current null)
             (whitespace-run null)
             (row null)
             (cell-count 0)
             (maybe-number? #t)
             (contains-whitespace? #f))
    (let ((c (read-char in)))
      (cond ((eof-object? c)
             (values
              (cons (->cell current maybe-number? contains-whitespace?) row)
              (add1 cell-count)))
            ((equal? c #\newline)
             ;; Recognize #\newline + #\return combinations
             (when (equal? (peek-char in) #\return) (read-char in))
             (values
              (cons (->cell current maybe-number? contains-whitespace?) row)
              (add1 cell-count)))
            ((equal? c #\return)
             ;; Recognize #\return + #\newline combinations
             (when (equal? (peek-char in) #\newline) (read-char in))
             (values
              (cons (->cell current maybe-number? contains-whitespace?) row)
              (add1 cell-count)))
            ((equal? c #\,)
             ;; NOTE: will discard last whitespace-run
             (loop null null (cons (->cell current maybe-number? contains-whitespace?) row) (add1 cell-count) #t #f))
            ((char-whitespace? c)
             (if (null? current)
                 ;; Discard whitespaces at the start of the string
                 (loop current '() row cell-count maybe-number? contains-whitespace?)
                 (loop current
                       (cons c whitespace-run)
                       row
                       cell-count
                       maybe-number?
                       contains-whitespace?)))
            ((equal? c #\")
             (define-values (s m w) (slurp-string in))
             (loop (append s whitespace-run current)
                   '()
                   row
                   cell-count
                   (and quoted-numbers? maybe-number? m)
                   (or contains-whitespace? w)))
            (#t
             (loop (cons c (append whitespace-run current))
                   '()
                   row
                   cell-count
                   (and maybe-number? (or (char-numeric? c) (char-punctuation? c)))
                   (or contains-whitespace? (not (null? whitespace-run)))))))))

;; Read a data frame from the INPUT port, by decoding CSV input.  IF HEADERS?
;; is true, the first row in INPUT becomes the names of the columns,
;; otherwise, the columns will be named "col1", "col2", etc.  The first row
;; defines the number of columns: if subsequent rows have fewer cells, they
;; are padded with #f, if it has more, they are silently truncated.  NA
;; determines the string that constitutes the "not available" value.
(define (read-csv input headers? na qn?)
  (define df (make-data-frame))
  (define series #f)
  (define na? (if (procedure? na) na (lambda (v) (equal? v na))))
  (define (decode cell) (if (na? cell) #f cell))
  (unless (eof-object? (peek-char input))
    (define-values (first-row-cells series-count) (parse-line input qn?))
    (if headers?
        (let ((index 1)
              (seen-header-names '()))
          (set! series
                (for/list ([h (reverse first-row-cells)])
                  ;; Gracefully handle series with empty header names
                  (let ((name (~a (decode h))))
                    (unless name
                      (set! name (~a "col" index))
                      (set! index (add1 index)))
                    (let loop ([suffix 1]
                               [seen? (member name seen-header-names)])
                      (when seen?
                        (let ([candidate (format "~a (~a)" name suffix)])
                          (if (member candidate seen-header-names)
                              (loop (add1 suffix) #t)
                              (set! name candidate)))))
                    (set! seen-header-names (cons name seen-header-names))
                    (make-series name #:capacity 100)))))
        (begin
          (set! series (for/list ([idx (in-range series-count)])
                         (make-series (format "col~a" idx) #:capacity 100)))
          (for ([s (in-list series)] [v (in-list (reverse first-row-cells))])
            (series-push-back s (decode v)))))
    (set! series (reverse series))
    (let row-loop ()
      (unless (eof-object? (peek-char input))
        (let-values ([(cells cell-count) (parse-line input qn?)])
          ;; Normally, a CSV file should have the same number of slots in each
          ;; line, if there are more slots than series, we discard extra ones,
          ;; if there is a shortfall, we add #f to the remaining series.
          (for ([series (in-list series)]
                [cell (in-list (cond ((= series-count cell-count) cells)
                                     ((< series-count cell-count)
                                      ;; to many cells, problem is they are at
                                      ;; the front (remember that cells are in
                                      ;; reverse order)
                                      (drop cells (- cell-count series-count)))
                                     (#t
                                      ;; To few cells, we need to pad them out
                                      ;; at the front.
                                      (append (make-list (- series-count cell-count) #f) cells))))])
            (series-push-back series (decode cell))))
        (row-loop)))
    (for ((s (in-list series)))
      (df-add-series df s)))
  df)

;; Read CSV data in a data frame from the INP which is either a port or a
;; string, in which case it is assumed to be a file name.  IF HEADERS?  is
;; true, the first row in INPUT becomes the names of the columns, otherwise,
;; the columns will be named "col1", "col2", etc.  The first row defines the
;; number of columns: if subsequent rows have fewer cells, they are padded
;; with #f, if it has more, they are silently truncated.  NA represents the
;; cell value to be replaced by the NA value in the data frame, by default
;; only empty cells are NA values, but this allows specifying an additional
;; string to represent NA values (some CSV exporters use "-" as the not
;; available value).
(define (df-read/csv inp #:headers? (headers? #t) #:na (na "") #:quoted-numbers? (qn? #f))
  (if (path-string? inp)
      ;; not 'text: we might read MAC text files on a Windows machine!
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-csv i headers? na qn?)))
      (read-csv inp headers? na qn?)))

;;............................................................. provides ....

(provide/contract
 (df-write/csv (->* (data-frame? (or/c path-string? output-port?))
                    (#:start exact-nonnegative-integer? #:stop exact-nonnegative-integer?)
                    #:rest (listof string?)
                    any/c))
 (df-read/csv (->* ((or/c path-string? input-port?))
                   (#:headers? boolean?
                    #:na (or/c any/c (-> any/c boolean?))
                    #:quoted-numbers? boolean?)
                   data-frame?)))
