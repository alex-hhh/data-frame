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

;; Parse a LINE from a CSV file and return the list of "cells" in it as
;; strings.  Takes special care that comma characters "," inside strings are
;; correctly handled.  Also double quotes inside strings are unquoted.
(define (parse-line in)
  (let loop ((c (read-char in))
             (current (open-output-string))
             (row '())
             (in-string? #f))
    (cond ((eof-object? c)
           (reverse (cons (get-output-string current) row)))
          ((equal? c #\newline)
           ;; Recognize #\newline + #\return combinations
           (when (equal? (peek-char in) #\return) (read-char in))
           (reverse (cons (get-output-string current) row)))
          ((equal? c #\return)
           ;; Recognize #\return + #\newline combinations
           (when (equal? (peek-char in) #\newline) (read-char in))
           (reverse (cons (get-output-string current) row)))
          ((and (eqv? c #\,) (not in-string?))
           (loop (read-char in)
                 (open-output-string)
                 (cons (get-output-string current) row)
                 #f))
          ((and in-string? (eqv? c #\") (eqv? (peek-char in) #\"))
           (write-char c current)
           (read-char in)             ; consume the next char
           (loop (read-char in) current row in-string?))
          ((eqv? c #\")
           (write-char c current)
           (loop (read-char in) current row (not in-string?)))
          (#t
           (write-char c current)
           (loop (read-char in) current row in-string?)))))

;; Decode the string VAL into a Racket value. The decoding rules are:
;;
;; * the trimmed string is passed to `na?` and if it returns #t, the value is
;; considered a NA
;;
;; * if the trimmed string is enclosed in quotes, it is assumed to be a string
;; and quotes are removed before passing the value to `na?`.  If
;; quote-numbers? is #t, the string is tried to be decoded as a number anyway.
;;
;; * if it parses as a number, return the number
;;
;; * otherwise return it as a string.
;;
(define (decode-value val na? quoted-numbers?)
  ;; string->number decodes #x #b and #o as hex, binary or octal. We could
  ;; also recognize 0x as hex, but we don't for now.
  (let* ((trimmed (string-trim val))
         (radix 10)
         (n (string-length trimmed)))
    (cond ((na? trimmed) #f)
          ((and
            (>= n 2)
            (eqv? #\" (string-ref trimmed 0))
            (eqv? #\" (string-ref trimmed (sub1 n))))
           (let ((v (substring trimmed 1 (sub1 n))))
             (cond ((na? v) #f)
                   (quoted-numbers? (or (string->number v radix) v))
                   (#t v))))
          (#t
           (or (string->number trimmed radix) trimmed)))))

;; Read a data frame from the INPUT port, by decoding CSV input.  IF HEADERS?
;; is true, the first row in INPUT becomes the names of the columns,
;; otherwise, the columns will be named "col1", "col2", etc.  The first row
;; defines the number of columns: if subsequent rows have fewer cells, they
;; are padded with #f, if it has more, they are silently truncated.  NA
;; determines the string that constitutes the "not available" value.
;;
;; if quoted-numbers? is #t, it is assumed that numeric values are also quoted
;; (e.g. "123") and will be decoded as numbers.  If it is #f, a value like
;; "123" will remain a string.
;; 
(define (read-csv input headers? na quoted-numbers?)
  (define df (make-data-frame))
  (define series #f)
  (define na? (if (procedure? na) na (lambda (v) (equal? v na))))
  (define (decode cell) (decode-value cell na? quoted-numbers?))
  (let loop ()
    (cond ((eof-object? (peek-char input))
           (for ((s (in-vector series)))
             (df-add-series df s))
           df)
          (#t
           (let ((slots (parse-line input)))
             (if series
                 ;; Normally, a CSV file should have the same number of slots
                 ;; in each line, if there are more slots than series, we
                 ;; discard extra ones, if there is a shortfall, we add #f to
                 ;; the remaining series.
                 (let ((shortfall (max 0 (- (vector-length series) (length slots)))))
                   (for ([s (in-vector series)]
                         [v (in-list (append slots (make-list shortfall "")))])
                     (series-push-back s (decode v))))
                 ;; First row
                 (if headers?
                     (let ((index 1))
                       (set! series
                             (for/vector ([h slots])
                               ;; Gracefully handle series with empty header names
                               (let ((name (~a (decode h))))
                                 (unless name
                                   (set! name (~a "col" index))
                                   (set! index (add1 index)))
                                 (make-series name)))))
                     (begin
                       (set! series (for/vector ([idx (in-range (length slots))])
                                      (make-series (format "col~a" idx))))
                       (for ([s (in-vector series)] [v (in-list slots)])
                         (series-push-back s (decode v)))))))
           (loop)))))

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
                    #:na (or/c string? (-> string? boolean?))
                    #:quoted-numbers? boolean?)
                   data-frame?)))
