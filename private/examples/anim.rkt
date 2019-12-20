#lang racket/gui
;;; lines.rkt
;;; Copyright (c)2011 M. Douglas Williams
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
;;; 1.0.0    11/12/11  Initial release. (MDW)

;;; 3D animated plot example.

(require plot
         animated-canvas)

(define (main delta t-last)
  (parameterize ((plot3d-samples 21))
    (let loop ((t 0.0))
      (define (f x y) (* (sin (* 2.0 x)) (sin (* 2.0 y)) (cos t)))
      (when (<= t t-last)
        (let* ((dc (send canvas get-dc))
               (width (send canvas get-width))
               (height (send canvas get-height)))
          (plot3d/dc (contour-intervals3d f 0 pi 0 pi)
                     dc 0 0 width height
                     #:z-min -1 #:z-max 1)
          (send canvas swap-bitmaps)
          (loop (+ t delta)))))))

(define frame
  (instantiate frame% ("Test Plot Animation")))

(define canvas
  (instantiate animated-canvas%
    (frame)
    (style '(border))
    (min-width 400)
    (min-height 300)))

(send frame show #t)

(main (* 0.05 pi) 20.0)
