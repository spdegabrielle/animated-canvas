#lang racket/gui
;;; histogram-test.rkt
;;; Copyright (c) 2007-2010 M. Douglas Williams
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

(require (planet williams/science/random-distributions)
         "histogram-widget.rkt")

(define (main n)
  (random-source-randomize! (current-random-source))
  (for ((i (in-range n)))
    (let ((gaussian (random-unit-gaussian))
          (tri (random-triangular 0.0 10.0 4.0)))
      (send histogram-1 set-value gaussian)
      (send histogram-2 set-value tri)
      (yield))))

(define frame
  (instantiate frame% ("Test Histogram Widget")))

(define histogram-1
  (instantiate histogram-widget%
    ("Unit Gaussian" 40 -4.0 4.0 frame)
    (min-width 400)
    (min-height 200)))

(define histogram-2
  (instantiate histogram-widget%
    ("Triangular" 40 0.0 10.0 frame)
    (min-width 400)
    (min-height 200)))

(send frame show #t)

(main 10000)