#lang racket/base

;; gpx.rkt -- read and write GPX files from data frames
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
         racket/format
         racket/match
         xml
         "df.rkt"
         "exn.rkt"
         "series.rkt"
         "xml-common.rkt")


;;......................................................... df-write/gpx ....

(define gpx-header-tag
  "<gpx xmlns=\"http://www.topografix.com/GPX/1/1\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" creator=\"ActivityLog2\" version=\"1.1\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd\">\n")

(define gpx-indent (make-parameter 0))
(define gpx-indent-string (make-parameter ""))
(define gpx-export-series (make-parameter '("timestamp" "lat" "lon" "alt")))

;; Convert the UTC timestamp (in seconds) into a string in the format
;; "YYYY-MM-DDTHH-MM-SSZ"
(define (seconds->gpx-timestamp utc)

  (define (fmt num width)
    (~a #:width width #:align 'right #:pad-string "0" num))

  (let ((d (seconds->date utc #f)))
    (string-append
     (fmt (date-year d) 4) "-" (fmt (date-month d) 2) "-" (fmt (date-day d) 2)
     "T"
     (fmt (date-hour d) 2) ":" (fmt (date-minute d) 2) ":" (fmt (date-second d) 2) "Z")))


;; Write to (current-output-port) the XML content for a GPX track point.
(define (gpx-emit-trkpt lat lon alt timestamp)
  (let ((indent (gpx-indent-string)))
    (write-string (format "~a<trkpt lat=\"~a\" lon=\"~a\">~%" indent lat lon))
    (write-string (format "~a  <ele>~a</ele>\n" indent alt))
    (write-string (format "~a  <time>~a</time>\n" indent (seconds->gpx-timestamp timestamp)))
    (write-string (format "~a</trkpt>\n" indent))))

;; Write to (current-output-port) the XML content for a GPX way point
(define (gpx-emit-wpt lat lon alt timestamp name)
  (let ((indent (gpx-indent-string)))
    (write-string (format "~a<wpt lat=\"~a\" lon=\"~a\">~%" indent lat lon))
    (write-string (format "~a  <name>~a</name>\n" indent name))
    (write-string (format "~a  <ele>~a</ele>\n" indent alt))
    (write-string (format "~a  <time>~a</time>\n" indent (seconds->gpx-timestamp timestamp)))
    (write-string (format "~a</wpt>\n" indent))))

;; Write to (current-output-port) the lap markers in DF as way points.
(define (gpx-emit-laps df)
  (define laps (or (df-get-property df 'laps) '()))
  (define limit (df-row-count df))
  (unless (null? laps)
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      ;; NOTE: don't emit the first lap, as that coincides with the start of
      ;; the track
      (for ([(lap lap-num) (in-indexed laps)] #:when (> lap-num 0))
        (match-define (vector timestamp lat lon ele)
          (df-lookup df "timestamp" (gpx-export-series) lap))
        (gpx-emit-wpt lat lon ele timestamp (format "Lap ~a" lap-num))))))

;; Write to (current-output-port) the GPS points in DF as a track.  The track
;; is named by the NAME parameter.
;;
;; NAME specifies the name of the track.  If it is #f, the 'name property of
;; DF is consulted and if that one is missing, a default name is used.
(define (gpx-emit-trk df (name #f))
  (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                  (gpx-indent-string (make-string (gpx-indent) #\ )))
    (write-string (format "~a<trk>\n" (gpx-indent-string)))
    (let ((name (or name (df-get-property df 'name) "GPX track")))
      (write-string (format "~a  <name>~a</name>\n" (gpx-indent-string) name)))
    (write-string (format "~a  <trkseg>\n" (gpx-indent-string)))
    (parameterize* ((gpx-indent (+ 2 (gpx-indent)))
                    (gpx-indent-string (make-string (gpx-indent) #\ )))
      (df-for-each
       df (gpx-export-series)
       (lambda (data)
         (when data
           (match-define (list timestamp lat lon calt) data)
           (when (and lat lon calt timestamp)
             (gpx-emit-trkpt lat lon calt timestamp))))))
    (write-string (format "~a  </trkseg>\n" (gpx-indent-string)))
    (write-string (format "~a</trk>\n" (gpx-indent-string)))))

;; Write the contents of DF in GPX format to the output port OUT.  See
;; `df-write/gpx` for some notes on what is actually written.
(define (write-gpx df out #:name (name #f))
  (unless (df-contains? df "timestamp" "lat" "lon")
    (df-raise "cannot export GPX track -- timestamp or lat or lon series missing"))
  (unless (df-contains/any? df "calt" "alt")
    (df-raise "cannot export GPX track -- altitude series missing"))
  (parameterize ((current-output-port out)
                 (gpx-indent 0)
                 (gpx-indent-string "")
                 (gpx-export-series
                  (cond ((df-contains? df "calt")
                         ;; Prefer corrected altitude
                         (list "timestamp" "lat" "lon" "calt"))
                        ((df-contains? df "alt")
                         ;; Fall back on recorded altitude, if that is
                         ;; available
                         (list "timestamp" "lat" "lon" "alt"))
                        (#t
                         (df-raise "cannot export GPX track -- alt or calt series missing")))))
    (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (write-string gpx-header-tag)
    (gpx-emit-laps df)
    (gpx-emit-trk df name)
    (write-string "</gpx>\n")))

;; Export the GPS track from the data frame DF to OUT -- which is either an
;; output port or a string, in which case it denotes a file name.  The data
;; frame is expected to contain the "timestamp", "lat", "lon" series, and
;; optionally "alt" or "calt" (corrected altitude) series.
;;
;; The entire GPS track is exported as a single track segment.
;;
;; The 'laps property, if present, is assumed to contain a list of timestamps.
;; The positions corresponding to these timestamps are exported as way points.
;;
;; The name of the segment can be specified as the NAME parameter. If this is
;; #f, the 'name property in the data frame is consulted, if that one is
;; missing a default track name is used.
;;
;; TODO: as an improvement, we could split the track around teleport points
;; into different segments.  This would work nicely when exporting skiing
;; runs.
;;
(define (df-write/gpx df outp #:name (name #f))
  (if (path-string? outp)
      (call-with-output-file outp
        #:mode 'text #:exists 'truncate/replace
        (lambda (o)
          (write-gpx df o #:name name)))
      (write-gpx df outp #:name name)))



;;.......................................................... df-read/gpx ....

(define (get-gpx-tree xml)
  (let ((e (document-element xml)))
    (if (eq? (element-name e) 'gpx)
        e
        (df-raise "not a gpx file"))))

(define (get-track gpx)
  (for/first ([e (element-content gpx)] #:when (e-name? e 'trk))
    e))

(define (get-track-name track)
  (for/first ([e (element-content track)] #:when (e-name? e 'name))
    (pcdata-string
     (for/first ([e (element-content e)] #:when (pcdata? e)) e))))

(define (get-track-segments track)
  (for/list ([e (element-content track)] #:when (e-name? e 'trkseg))
    e))

(define (count-track-points track)
  (for/sum ([e (element-content track)] #:when (e-name? e 'trkpt))
    1))

(define (first-trkpt track)
  (for/first ([e (element-content track)] #:when (e-name? e 'trkpt))
    e))

(define (parse-track-point trkpt)
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
    (for ([a (element-attributes trkpt)])
      (case (attribute-name a)
        ((lat) (set! lat (string->number (attribute-value a))))
        ((lon) (set! lon (string->number (attribute-value a))))))
    ;; NOTE: we should extract the timestamp perhaps?  we don't really care
    ;; about it for now...
    (for ([e (element-content trkpt)] #:when (element? e))
      (let ((data (get-pcdata e)))
        (case (element-name e)
          ((time) (set! timestamp (xml-timestamp->seconds data)))
          ((ele) (set! elevation (string->number data)))
          ((extensions)
           (for ([e (element-content e)] #:when (element? e))
             (let ((data (get-pcdata e)))
               (case (element-name e)
                 ((gpxtpx:hr gpxdata:hr heartrate) (set! hr (and data (string->number data))))
                 ((gpxtpx:cad gpxdata:cadence cadence) (set! cadence (and data (string->number data))))
                 ((gpxtpx:atemp gpxdata:temp) (set! temp (and data (string->number data))))
                 ((gpxdata:speed) (set! speed (and data (string->number data))))
                 ((gpxdata:power power) (set! power (and data (string->number data))))
                 ((gpxdata:distance) (set! distance (and data (string->number data))))
                 ((gpxtpx:TrackPointExtension)
                  (for ([e (element-content e)] #:when (element? e))
                    (let ((data (get-pcdata e)))
                      (case (element-name e)
                        ((gpxtpx:hr) (set! hr (and data (string->number data))))
                        ((gpxtpx:cad) (set! cadence (and data (string->number data))))
                        ((gpxtpx:atemp) (set! temp (and data (string->number data))))))))
                 ((gpxpx:PowerInWatts)
                  (set! power (and data (string->number data)))))))))))
    (list timestamp lat lon elevation distance speed temp hr cadence power)))

(define (parse-way-point wpt)
  (let ((lat #f)
        (lon #f)
        (timestamp #f)
        (elevation #f)
        (name #f))
    (for ([a (element-attributes wpt)])
      (case (attribute-name a)
        ((lat) (set! lat (string->number (attribute-value a))))
        ((lon) (set! lon (string->number (attribute-value a))))))
    ;; NOTE: we should extract the timestamp perhaps?  we don't really care
    ;; about it for now...
    (for ([e (element-content wpt)] #:when (element? e))
      (let ((data (pcdata-string
                   (for/first ([e (element-content e)] #:when (pcdata? e)) e))))
        (case (element-name e)
          ((time) (set! timestamp (xml-timestamp->seconds data)))
          ((ele) (set! elevation (string->number data)))
          ((name) (set! name data)))))
    (list timestamp lat lon elevation name)))

(define (parse-all-way-points gpx)
  (for/list ([e (element-content gpx)] #:when (e-name? e 'wpt))
    (parse-way-point e)))

;; Find the timestamp for the DF row that contains the point that is closest
;; to the LAT + LON position.  Note that this is a simplistic method and will
;; produce incorrect results if the GPX track contains loops or it is an "out
;; and back" track.
(define (get-closest-timestamp df lat lon)
  (and (df-contains? "lat" "lon" "timestamp")
       (let-values (((timestamp distance)
                     (for/fold ([timestamp #f] [distance #f])
                               (([plat plon ts] (in-data-frame df "lat" "lon" "timestamp")))
                       (if (and plat plon)
                           (let ((dst (map-distance/degrees plat plon lat lon)))
                             (if (or (not timestamp) (< dst distance))
                                 (values ts dst)
                                 (values timestamp distance)))
                           (values timestamp distance)))))
         timestamp)))

;; Construct a data frame from the GPX document specified as an input port.
;; See `df-read/gpx` for details on what is read from the document.
(define (read-gpx in)
  (define gpx (get-gpx-tree (slurp-xml in)))
  (define track (get-track gpx))
  (unless track (df-raise "could not find track"))
  (define track-segments (get-track-segments track))
  (define item-count
    (for/sum ([segment (in-list track-segments)])
      (count-track-points segment)))
  (define lat (make-vector item-count))
  (define lon (make-vector item-count))
  (define alt (make-vector item-count))
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

  (define index 0)
  (for* ([s (in-list track-segments)]
         [e (in-list (element-content s))] #:when (e-name? e 'trkpt))
    (match-define (list pt-timestamp pt-lat pt-lon pt-ele pt-distance
                        pt-speed pt-temp pt-hr pt-cadence pt-power) (parse-track-point e))

    (vector-set! timestamp index pt-timestamp)
    (vector-set! lat index pt-lat)
    (vector-set! lon index pt-lon)
    (vector-set! alt index pt-ele)

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

    (set! index (add1 index)))

  (define lat-col (make-series "lat" #:data lat))
  (define lon-col (make-series "lon" #:data lon))

  (define df (make-data-frame))
  (df-add-series! df lat-col)
  (df-add-series! df lon-col)

  (when (for/first ([e (in-vector timestamp)] #:when e) e)
    ;; We have timestamps in this GPX file, but they may or may not be sorted.
    ;; Create an appropriate series and add it to the data frame
    (define ts
      (with-handlers
        (((lambda (e) #t) (lambda (e) (make-series "timestamp" #:data timestamp))))
        (make-series "timestamp" #:data timestamp #:cmpfn <=)))
    (df-add-series! df ts))

  (define (maybe-add name data)
    (when (and data (for/first ([e (in-vector data)] #:when e) e))
      (df-add-series! df (make-series name #:data data))))

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
    (df-set-sorted! df "dst" <=))

  (define track-name (get-track-name track))
  (when track-name
    (df-put-property! df 'name track-name))
  (define waypoints (parse-all-way-points gpx))
  ;; Try to reconstruct the 'laps property from the way points -- this will
  ;; not work very well if the way points don't have a timestamp and the GPX
  ;; track contains loops or it is an "out and back" track.
  (define laps
    (for/list ([wpt (in-list waypoints)])
      (match-define (list timestamp lat lon elevation name) wpt)
      (or timestamp (get-closest-timestamp df lat lon))))
  (df-put-property! df 'waypoints waypoints)
  (df-put-property! df 'laps (filter values laps))

  df)

;; Construct a data frame from the GPX document specified in INP -- which is
;; either an input port or a string, in which case it denotes an input file.
;; The data frame will have "timestamp", "lat", "lon", "alt", "dst" and other
;; series.  See doc/session-df.md for the meaning of these series.
;;
;; The data frame will also have the following properties:
;;
;; * a 'name property containing the "NAME" of the track segment, if this is
;;   present in the GPX file.
;;
;; * a 'waypoints property containing a list of waypoints, if they GPX track
;;   has any.  Each waypoint is represented as a list of TIMESTAMP, LAT, LON,
;;   ELEVATION and NAME
;;
;; * a 'laps property containing a list of timestamps corresponding to each
;;   way point in the waypoint list -- the laps property cannot be constructed
;;   correctly if the waypoints are missing a timestamp property.
;;
;; All track segments in the GPX document are concatenated into a single
;; segment.
(define (df-read/gpx inp)
  (if (path-string? inp)
      (call-with-input-file inp #:mode 'text
        (lambda (i) (read-gpx i)))
      (read-gpx inp)))


;;............................................................. provides ....

(provide/contract
 (df-write/gpx (->* (data-frame? (or/c path-string? output-port?))
                    (#:name (or/c #f string?))
                    any/c))
 (df-read/gpx (-> (or/c path-string? input-port?) data-frame?)))
