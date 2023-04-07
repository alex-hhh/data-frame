#lang racket/base

;; slr.rkt -- simple linear regression utilities
;;
;; This file is part of data-frame -- https://github.com/alex-hhh/data-frame
;; Copyright (c) 2018, 2020, 2023 Alex Harsányi <AlexHarsanyi@gmail.com>
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

(require math/statistics
         racket/contract)

;; Simple linear regression parameters Y = alpha + beta * X.  r is the
;; correlation coefficient.
(struct slr (alpha beta r) #:transparent)

;; Compute linear regression parameters for the list of samples XS, YS,
;; optionally weighted by WS.
;;
;; https://en.wikipedia.org/wiki/Simple_linear_regression
(define (simple-linear-regression xs ys (ws #f))
  (let ((x-stats (update-statistics* empty-statistics xs ws))
        (y-stats (update-statistics* empty-statistics ys ws))
        (r (correlation xs ys)))
    (let* ((beta (* r (/ (statistics-stddev y-stats) (statistics-stddev x-stats))))
           (alpha (- (statistics-mean y-stats) (* beta (statistics-mean x-stats)))))
      ;; NOTE: +nan.0 and +inf.0 are numbers as far as `number?` is concerned.
      (and (rational? alpha) (rational? beta) (rational? r) (slr alpha beta r)))))


;;............................................................. provides ....

(provide (struct-out slr))

(provide/contract
 (simple-linear-regression
  (->* ((or/c (listof number?) (vectorof number?))
        (or/c (listof number?) (vectorof number?)))
       ((or/c (listof number?) (vectorof number?)))
       (or/c slr? #f))))
