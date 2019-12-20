#lang racket/gui
;;; animated-bubble-sort.rkt
;;; Copyright (c)2012 M. Douglas Williams
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;; ------------------------------------------------------------------------------
;;;
;;; Version  Date      Description
;;; 1.0.0    11/18/12  Initial release. (MDW)

;;; Animated insertion sort example.

(require plot
         animated-canvas)

(define (plot-vector v canvas)
  (define dc (send canvas get-dc))
  (define width (send canvas get-width))
  (define height (send canvas get-height))
  (parameterize ((plot-decorations? #f))
    (plot/dc (discrete-histogram 
              (for/list ((item (in-vector v))
                         (i (in-naturals)))
                (vector i item)))
             dc 0 0 width height))
  (send canvas swap-bitmaps)
  (yield))

(define (animated-bubble-sort v)
  (plot-vector v canvas)
  (for ((i (in-range 0 (- (vector-length v) 1))))
    (for ((j (in-range (- (vector-length v) 1) i -1)))
      (when (< (vector-ref v j) (vector-ref v (- j 1)))
        (define temp (vector-ref v j))
        (vector-set! v j (vector-ref v (- j 1)))
        (vector-set! v (- j 1) temp)
        (plot-vector v canvas)))))

(define (main)
  (define array
    (build-vector
     50
     (lambda (i)
       (random 100))))
  (animated-bubble-sort array))

(define frame
  (instantiate frame% ("Bubble Sort Animation")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style '(border))
    (min-width 600)
    (min-height 300)))

(send frame show #t)

(main)
