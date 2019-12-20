#lang racket/gui
;;; animated-insertion-sort.rkt
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

(define (animated-insertion-sort v)
  (plot-vector v canvas)
  (for ((j (in-range 1 (vector-length v))))
    (define key (vector-ref v j))
    (define i (- j 1))
    (let loop ()
      (when (and (>= i 0)
                 (> (vector-ref v i) key))
        (vector-set! v (+ i 1) (vector-ref v i))
        (plot-vector v canvas)
        (set! i (- i 1))
        (loop)))
    (vector-set! v (+ i 1) key)
    (plot-vector v canvas)))

(define (main)
  (define array
    (build-vector
     50
     (lambda (i)
       (random 100))))
  (animated-insertion-sort array))

(define frame
  (instantiate frame% ("Insertion Sort Animation")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style '(border))
    (min-width 600)
    (min-height 300)))

(send frame show #t)

(main)
