#lang racket/gui
;;; histogram-widget.rkt
;;; Copyright (c) 2007-2011 M. Douglas Williams
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
;;; 2.0.0    06/01/10  Updated for Racket. (MDW)
;;; 2.1.0    11/12/11  Updated to use the new plot collection. (MDW)

(require plot
         animated-canvas
         (planet williams/science/histogram))

(define histogram-widget%
  (class vertical-panel%
    ;; Init parameters
    (init-field label)
    (init-field n)
    (init-field min-range)
    (init-field max-range)
    (init parent)
    (init (font normal-control-font))
    ;; Instantiate superclass
    (super-instantiate (parent))
    ;; Create graphical subelements
    (define message
      (instantiate message%
        (label this)))
    (define canvas 
      (instantiate animated-canvas%
        (this)
        (style '(border))))
    ;; Recompute sizes and positions
    (send this reflow-container)
    ;; Create histogram vector
    (define histogram
      (make-histogram-with-ranges-uniform n min-range max-range))
    ;; Draw histogram
    (define (draw-histogram (scale 1))
      (let* ((dc (send canvas get-dc))
             (width (send canvas get-width))
             (height (send canvas get-height))
             (n (histogram-n histogram))
             (bins (histogram-bins histogram))
             (ranges (histogram-ranges histogram))
             (rects
              (for/list ((i (in-range n)))
                (let ((x0 (vector-ref ranges i))
                      (x1 (vector-ref ranges (+ i 1)))
                      (y (vector-ref bins i)))
                  (vector (ivl x0 x1) (ivl 0 y))))))
        (plot/dc (rectangles rects)
                 dc 0 0 width height
                 #:x-min (vector-ref ranges 0)
                 #:x-max (vector-ref ranges (- (vector-length ranges) 1))
                 #:x-label "x"
                 #:y-min 0
                 #:y-max (histogram-max histogram)
                 #:y-label "Count"))
      (send canvas swap-bitmaps))
    ;; Reset method
    (define/public (reset)
      (set! histogram
            (make-histogram-with-ranges-uniform n min-range max-range))
      (draw-histogram))
    ;; Set value method
    (define/public (set-value value)
      (histogram-increment! histogram value)
      (draw-histogram))))

;;; Module Contracts

(provide (all-defined-out))
