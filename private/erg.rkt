#lang racket/base

;; erg.rkt -- read ERG files
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

(require racket/string
         racket/contract
         racket/dict
         racket/match
         "df.rkt"
         "series.rkt"
         )

(define rx-chstart #px"^\\s*\\[\\s*(?i:course\\s+header)\\s*\\]")
(define rx-kvdef #px"^\\s*(.*?)\\s*=\\s*(.*?)\\s*$")
(define rx-chend #px"^\\s*\\[\\s*(?i:end\\s+course\\s+header)\\s*\\]")
(define rx-cdstart #px"^\\s*\\[\\s*(?i:course\\s+data)\\s*\\]")
(define rx-cdend #px"^\\s*\\[\\s*(?i:end course\\s+data)\\s*\\]")

(define (read-course-header port)
  ;; Skip lines until course header is found
  (let loop ([line (read-line port 'any)])
    (cond ((eof-object? line)
           (error "Failed to find course header start"))
          ((regexp-match rx-chstart line)
           #t)
          (#t (loop (read-line port 'any)))))
  (let loop ([line (read-line port 'any)]
             [kv '()])
    (cond ((eof-object? line)
           (error "Failed to find course header end"))
          ((regexp-match rx-chend line)
           kv)
          ((regexp-match rx-kvdef line)
           =>
           (lambda (v)
             (loop (read-line port 'any)
                   (cons (cons (list-ref v 1) (list-ref v 2)) kv))))
          (#t
           (loop (read-line port 'any)
                 (cons (cons "DATA-SERIES" (string-split line)) kv)))
          )))

(define (read-course-data port)
  ;; Skip lines until course data is found
  (let loop ([line (read-line port 'any)])
    (cond ((eof-object? line)
           (error "Failed to find course header start"))
          ((regexp-match rx-cdstart line)
           #t)
          (#t (loop (read-line port 'any)))))
  (let loop ([line (read-line port 'any)]
             [data '()])
    (cond ((eof-object? line)
           (error "Failed to find course data end"))
          ((regexp-match rx-cdend line)
           (reverse data))
          (#t
           (loop (read-line port 'any) (cons line data))))))

(define (read-erg in)
  (define kv (read-course-header in))
  (define series (dict-ref kv "DATA-SERIES"
                           (lambda () (error "missing data series definition"))))
  (when (or (< (length series) 2) (> (length series) 2))
    (error "wrong data series definition"))
  (define data-lines (read-course-data in))
  (define item-count (length data-lines))
  (define data (for/list ([i (in-range item-count)]) (make-vector item-count #f)))
  (for ([(line index) (in-indexed (in-list data-lines))])
    (for ([item (in-list (string-split line #px"\\s+" #:trim? #t))]
          [v (in-list data)])
      (vector-set! v index (string->number item))))
  (define df (make-data-frame))
  (for ([s series] [d data])
    (df-add-series df (make-series s #:data d)))
  (for ([kv (in-list kv)] #:unless (equal? "DATA-SERIES" (car kv)))
    (df-put-property df (string->symbol (string-downcase (car kv)))
                     (or (string->number (cdr kv)) (cdr kv))))
  (when (df-contains? df "MINUTES")
    ;; NOTE: this might fail if MINUTES contains invalid values or it is not
    ;; sorted.
    (df-set-sorted df "MINUTES" <=))

  ;; Some ERG files use "percentage" instead of "percent".  Create a PERCENT
  ;; series, so the rest of the software does not have to worry about this,
  ;; but keep the PERCENTAGE as well.
  (when (df-contains? df "PERCENTAGE")
    (df-add-derived df "PERCENT" '("PERCENTAGE")
                    (lambda (x) (list-ref x 0))))

  ;; Some ERG files have a MINUTES WATTS series, but if there is a FTP field
  ;; defined, we can construct a PERCENTAGE series, which means the ERG can be
  ;; converted to any other FTP.
  (unless (df-contains? df "PERCENT")
    (define ftp (df-get-property df 'ftp))
    (when (and ftp (number? ftp))
      (erg-add-percent-series df ftp)))
  df)

(define (erg-add-percent-series df ftp)
  (unless (df-contains? df "WATTS")
    (error "erg-add-percentage-series: erg file has no power series"))
  (df-put-property df 'ftp ftp)
  (df-add-derived df "PERCENT" '("WATTS")
                  (lambda (x)
                    (define power (list-ref x 0))
                    (and power (* (/ power ftp) 100.0)))))

(define (df-read/erg inp)
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text read-erg)
      (read-erg inp)))

(define (erg-intervals df)
  (define intervals '())
  (define time 0)
  (df-for-each
   df
   '("MINUTES" "PERCENT")
   (lambda (o n)
     (when o
       (match-define (list otime opct) o)
       (match-define (list ntime npct) n)
       (define delta (- ntime otime))
       (when (> delta 0)
         (set! intervals (cons (list time delta opct npct) intervals))
         (set! time (+ time delta))))))
  (reverse intervals))

(provide/contract
 (df-read/erg (-> (or/c path-string? input-port?) data-frame?))
 (erg-add-percent-series (-> data-frame? (and/c number? positive?) any/c)))

