#lang racket/base
;; xml-common.rkt -- common utilities for reading GPX and TPX data
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
         racket/date
         racket/flonum
         racket/math
         racket/match
         xml)


;;................................................. map-distance/degrees ....

;; Formulas from http://www.movable-type.co.uk/scripts/latlong.html

(define earth-radius (->fl 6371000))    ; meters

(define (haversin theta)
  (fl/ (fl- 1.0 (flcos theta)) 2.0))

(define (inv-haversin h)
  (fl* 2.0 (flasin (flsqrt h))))

;; Calculate the distance in meters between two map coordinates
(define (map-distance/radians lat1 lon1 lat2 lon2)
  (let ((delta-lat (fl- lat2 lat1))
        (delta-lon (fl- lon2 lon1)))
    (let* ((a (fl+ (haversin delta-lat)
                   (fl* (fl* (flcos lat1) (flcos lat2))
                        (haversin delta-lon))))
           (c (inv-haversin a)))
      (fl* c earth-radius))))

(define (map-distance/degrees lat1 lon1 lat2 lon2)
  (map-distance/radians
   (degrees->radians lat1)
   (degrees->radians lon1)
   (degrees->radians lat2)
   (degrees->radians lon2)))

(define timestamp-rx
  #px"([[:digit:]]+)-([[:digit:]]+)-([[:digit:]]+)T([[:digit:]]+):([[:digit:]]+):([[:digit:]]+(.[[:digit:]]+)?)Z")

;; Parse a timestamp in the format "YYYY-MM-DDTHH-MM-SS.SSSZ" and return the
;; UTC seconds corresponding to that timestamp. Note a fractional timestamp
;; might be returned, if the string contains a fractional timestamp
(define (xml-timestamp->seconds ts)
  (match (regexp-match timestamp-rx ts)
    ((list _ year month day hour minute seconds rest ...)
     (define s (string->number seconds))
     (define fractional (- s (exact-truncate s)))
     (+ fractional (date->seconds
                    (make-date (exact-truncate s)
                               (string->number minute)
                               (string->number hour)
                               (string->number day)
                               (string->number month)
                               (string->number year)
                               ;; These are ignored by date->seconds
                               0 0 #f 0)
                    #f)))
    (_ #f)))

;; Convenience function to check if E is an XML document by NAME
(define (e-name? e name)
  (and (element? e) (eq? (element-name e) name)))

;; Convenience function to get the contents of an XML element
(define (get-pcdata e)
  (let ([data (for/first ([e (element-content e)] #:when (pcdata? e)) e)])
    (and data (pcdata-string data))))

;; Read the GPX XML document from the input port IN.  While reading the XML
;; contents, white space is collapsed and comments are skipped.
(define (slurp-xml in)
  (parameterize ((collapse-whitespace #t)
                 (read-comments #f)
                 ;; This is unintuitive, setting xml-count-bytes to #t will
                 ;; prevent line counting on the input port, and will speed up
                 ;; parsing, we'll be missing locations in the xml elements,
                 ;; but they are not used anyway.
                 (xml-count-bytes #t))
    (read-xml/document in)))

(provide/contract
 (map-distance/degrees (-> real? real? real? real? real?))
 (xml-timestamp->seconds (-> string? real?))
 (e-name? (-> any/c symbol? boolean?))
 (get-pcdata (-> element? (or/c string? #f)))
 (slurp-xml (-> input-port? document?)))
