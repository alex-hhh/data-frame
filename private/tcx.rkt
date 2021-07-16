#lang racket/base

;; tcx.rkt -- read TCX files into data frames
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
         racket/format
         racket/match
         racket/math
         xml
         "df.rkt"
         "exn.rkt"
         "series.rkt"
         "xml-common.rkt")

;; Parse a trackpoint XML node and return a list of data values from it.  The
;; same list is always returned, and if some data is missing (e.g. no power
;; data), the corresponding value is #f.
(define (parse-track-point trackpoint)
  (let ((lat #f)
        (lon #f)
        (timestamp #f)
        (elevation #f)
        (temp #f)
        (hr #f)
        (cadence #f)
        (speed #f)
        (power #f)
        (distance #f))
    (for ([e (element-content trackpoint)] #:when (element? e))
      (let ((data (get-pcdata e)))
        (case (element-name e)
          ((Time) (set! timestamp (xml-timestamp->seconds data)))
          ((AltitudeMeters) (set! elevation (and data (string->number data))))
          ((DistanceMeters) (set! distance (and data (string->number data))))
          ((Cadence) (set! cadence (and data (string->number data))))
          ((HeartRateBpm)
           (for ([e (element-content e)] #:when (element? e))
             (let ((data (get-pcdata e)))
               (case (element-name e)
                 ((Value) (set! hr (and data (string->number data))))))))
          ((Position)
           (for ([e (element-content e)] #:when (element? e))
             (let ((data (get-pcdata e)))
               (case (element-name e)
                 ((LatitudeDegrees) (set! lat (and data (string->number data))))
                 ((LongitudeDegrees) (set! lon (and data (string->number data))))))))
          ((Extensions)
           (for ([e (element-content e)]
                 #:when (and (element? e)
                             ;; NOTE: we should do XML namespace expansion here...
                             (regexp-match #rx"(^|:)TPX$" (symbol->string (element-name e)))))
             (for ([e (element-content e)] #:when (element? e))
               (let ((data (get-pcdata e))
                     (name (symbol->string (element-name e))))
                 (cond ((regexp-match #rx"(^|:)Speed" name)
                        (set! speed (and data (string->number data))))
                       ((regexp-match #rx"(^|:)Watts" name)
                        (set! power (and data (string->number data))))))))))))
    (list timestamp lat lon elevation distance speed temp hr cadence power)))

;; Return the number of track points in this ACTIVITY XML node.  We need this
;; to be able to pre-allocate the vectors which will hold the data.
(define (count-activity-track-points activity)
  (for/sum ([e (element-content activity)] #:when (e-name? e 'Lap))
    (for/sum ([e (element-content e)] #:when (e-name? e 'Track))
      (for/sum ([e (element-content e)] #:when (e-name? e 'Trackpoint))
        1))))

;; Construct a data frame from a TCX Activity XML node.  Data series are
;; constructed for the data points in each track point and the lap start
;; timestamps are added as the 'laps property.
(define (tcx-activity->data-frame activity)
  (define laps '())

  ;; These will be properties on the data frame (if present in the TCX file
  (define unit-id #f)
  (define product-id #f)
  (define sport #f)

  (for ([e (element-content activity)] #:when (e-name? e 'Creator))
    (for ([e (element-content e)] #:when (element? e))
      (let ((data (get-pcdata e)))
        (case (element-name e)
          ((UnitId) (set! unit-id (and data (or (string->number data) data))))
          ((ProductID) (set! product-id (and data (or (string->number data) data))))))))
  (for ([a (element-attributes activity)])
    (case (attribute-name a)
      ((Sport) (set! sport (attribute-value a)))))
  
  (define item-count (count-activity-track-points activity))

  (define timestamp (make-vector item-count #f))

  ;; These are optional series, so we delay creating the vector that stores
  ;; the values until the first element in the series is seen.  The trade-off
  ;; is that we have to check the presence of this vector for every item, so
  ;; we are sacrificing lower memory use for slower execution speed.
  (define temperature #f)
  (define heart-rate #f)
  (define cadence #f)
  (define power #f)
  (define speed #f)
  (define distance #f)
  (define lat #f)
  (define lon #f)
  (define alt #f)

  (define index 0)
  
  (for ([e (element-content activity)] #:when (e-name? e 'Lap))
    (define start-time
      (for/first ([a (element-attributes e)] #:when (equal? (attribute-name a) 'StartTime))
        (xml-timestamp->seconds (attribute-value a))))
    (when start-time
      (set! laps (cons start-time laps)))
    (for ([e (element-content e)] #:when (e-name? e 'Track))
      (for ([e (element-content e)] #:when (e-name? e 'Trackpoint))
        (match-define (list pt-timestamp pt-lat pt-lon pt-ele pt-distance
                            pt-speed pt-temp pt-hr pt-cadence pt-power) (parse-track-point e))

        (vector-set! timestamp index pt-timestamp)

        (when pt-lat
          (unless lat (set! lat (make-vector item-count #f)))
          (vector-set! lat index pt-lat))

        (when pt-lon
          (unless lon (set! lon (make-vector item-count #f)))
          (vector-set! lon index pt-lon))

        (when pt-ele
          (unless alt (set! alt (make-vector item-count #f)))
          (vector-set! alt index pt-ele))

        (when pt-temp
          (unless temperature (set! temperature (make-vector item-count #f)))
          (vector-set! temperature index pt-temp))

        (when pt-hr
          (unless heart-rate (set! heart-rate (make-vector item-count #f)))
          (vector-set! heart-rate index pt-hr))

        (when pt-cadence
          (unless cadence (set! cadence (make-vector item-count #f)))
          (vector-set! cadence index pt-cadence))

        (when pt-power
          (unless power (set! power (make-vector item-count #f)))
          (vector-set! power index pt-power))

        (when pt-speed
          (unless speed (set! speed (make-vector item-count #f)))
          (vector-set! speed index pt-speed))

        (when pt-distance
          (unless distance (set! distance (make-vector item-count #f)))
          (vector-set! distance index pt-distance))

        (set! index (add1 index)))))

  (define df (make-data-frame))

  (when product-id
    (df-put-property! df 'product-id product-id))
  (when unit-id
    (df-put-property! df 'unit-id unit-id))
  (when sport
    ;; Use the key tcx-sport to avoid confusion with sport which is used by
    ;; ActivityLog2 data frames
    (df-put-property! df 'tcx-sport sport))
  
  (when (for/first ([e (in-vector timestamp)] #:when e) e)
    ;; We have timestamps in this TCX file, but they may or may not be sorted.
    ;; Create an appropriate series and add it to the data frame
    (define ts
      (with-handlers
        (((lambda (e) #t) (lambda (e) (make-series "timestamp" #:data timestamp))))
        (make-series "timestamp" #:data timestamp #:cmpfn <)))
    (df-add-series! df ts))

  (define (maybe-add name data)
    (when (and data (for/first ([e (in-vector data)] #:when e) e))
      (df-add-series! df (make-series name #:data data))))

  (maybe-add "lat" lat)
  (maybe-add "lon" lon)
  (maybe-add "alt" alt)
  (maybe-add "temp" temperature)
  (maybe-add "hr" heart-rate)
  (maybe-add "cad" cadence)
  (maybe-add "pwr" power)
  (maybe-add "spd" speed)
  (maybe-add "dst" distance)

  ;; A "dst" series must exist in the data frame, so we create it if one was
  ;; not present in the data file.
  (when (and (df-contains? df "lat" "lon") (not (df-contains? df "dst")))
    (define dst 0)
    (df-add-derived!
     df
     "dst"
     '("lat" "lon")
     (lambda (prev next)
       (when (and prev next)
         (match-define (list prev-lat prev-lon) prev)
         (match-define (list next-lat next-lon) next)
         (when (and prev-lat prev-lon next-lat next-lon)
           (set! dst (+ dst (map-distance/degrees prev-lat prev-lon next-lat next-lon)))))
       dst)))

  ;; If the distance data came from the GPX file, it might not be in ascending
  ;; order, and marking it as sorted will fail, ignore such a failure.
  (with-handlers
    (((lambda (e) #t) (lambda (e) (void))))
    (df-set-sorted! df "dst" <))

  (df-put-property! df 'laps (reverse laps))

  df)

(define (get-tcx-root xml)
  (let ([e (document-element xml)])
    (if (eq? (element-name e) 'TrainingCenterDatabase)
        e
        (error "not a tcx file"))))

(define (get-tcx-activities tcx-root)
  (for/first ([e (element-content tcx-root)] #:when (e-name? e 'Activities))
    e))

(define (read-tcx in)
  (define tcx (get-tcx-root (slurp-xml in)))
  (for/first ([a (element-content (get-tcx-activities tcx))] #:when (e-name? a 'Activity))
    (tcx-activity->data-frame a)))

(define (read-tcx-multiple in)
  (define tcx (get-tcx-root (slurp-xml in)))
  (for/list ([a (element-content (get-tcx-activities tcx))] #:when (e-name? a 'Activity))
    (tcx-activity->data-frame a)))

;; Construct a data frame from the first activity in the TCX document
;; specified in INP -- which is either an input port or a string, in which
;; case it denotes an input file.  The data frame will have "timestamp",
;; "lat", "lon", "alt", "dst" and other series.  See doc/session-df.md for the
;; meaning of these series.
;;
;; The data frame will also have the following properties:
;;
;; * a 'laps property containing a list of timestamps corresponding to the
;; start time of each lap
(define (df-read/tcx inp)
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-tcx i)))
      (read-tcx inp)))

;; Construct multiple data frames from a TCX document -- one data frame for
;; each activity.  See also `df-read/tcx`.
(define (df-read/tcx/multiple inp)
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-tcx-multiple i)))
      (read-tcx-multiple inp)))


;............................................................. provides ....

(provide/contract
 (df-read/tcx (-> (or/c path-string? input-port?) data-frame?))
 (df-read/tcx/multiple (-> (or/c path-string? input-port?) (listof data-frame?))))
