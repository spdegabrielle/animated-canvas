#lang racket/gui
;;; lines.rkt
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

;;; Lines animated canvas example.
;;; Draws moving line similar to those in the Qix video game.

(require animated-canvas)

(define-struct qix-segment
  (x1 y1 x2 y2))

(define-struct qix
  (x1 y1 x-dot1 y-dot1 x2 y2 x-dot2 y-dot2 color segments)
  #:mutable)

(define (random-color)
  (make-object color% (random 256) (random 256) (random 256)))

(define max-segments 12)

(define (draw-qix the-qix)
  (define (draw-segment dc x1 y1 x2 y2 color)
    (send dc set-pen color 0 'solid)
    (send dc draw-line x1 y1 x2 y2))
  (let ((dc (send canvas get-dc)))
    (for-each
     (lambda (qix)
       (for-each
        (lambda (segment)
          (draw-segment dc
                        (qix-segment-x1 segment) (qix-segment-y1 segment)
                        (qix-segment-x2 segment) (qix-segment-y2 segment)
                        (qix-color qix)))
        (qix-segments qix)))
     the-qix)
    (send canvas swap-bitmaps)))

(define (move-qix the-qix)
  (define (update-position-x x x-dot)
    (set! x (+ x x-dot))
    (cond ((< x 0)
           (set! x (- x))
           (set! x-dot (- x-dot)))
          ((> x (send canvas get-width))
           (set! x (- (* 2 (send canvas get-width)) x))
           (set! x-dot (- x-dot))))
    (values x x-dot))
  (define (update-position-y y y-dot)
    (set! y (+ y y-dot))
    (cond ((< y 0)
           (set! y (- y))
           (set! y-dot (- y-dot)))
          ((> y (send canvas get-height))
           (set! y (- (* 2 (send canvas get-height)) y))
           (set! y-dot (- y-dot))))
    (values y y-dot))
  (for-each
   (lambda (qix)
     ;; Update the qix position
     (let-values (((x x-dot)(update-position-x (qix-x1 qix) (qix-x-dot1 qix))))
       (set-qix-x1! qix x)
       (set-qix-x-dot1! qix x-dot))
     (let-values (((y y-dot)(update-position-y (qix-y1 qix) (qix-y-dot1 qix))))
       (set-qix-y1! qix y)
       (set-qix-y-dot1! qix y-dot))
     (let-values (((x x-dot)(update-position-x (qix-x2 qix) (qix-x-dot2 qix))))
       (set-qix-x2! qix x)
       (set-qix-x-dot2! qix x-dot))
     (let-values (((y y-dot)(update-position-y (qix-y2 qix) (qix-y-dot2 qix))))
       (set-qix-y2! qix y)
       (set-qix-y-dot2! qix y-dot))
     ;; Add a new segment to the segment list
     (set-qix-segments! qix (append (qix-segments qix)
                                    (list (make-qix-segment 
                                           (qix-x1 qix) (qix-y1 qix)
                                           (qix-x2 qix) (qix-y2 qix)))))
     ;; Remove old segments
     (when (> (length (qix-segments qix)) max-segments)
       (set-qix-segments! qix (cdr (qix-segments qix)))))
   the-qix))

(define break? #f)

(define (main n-qix)
  (begin-busy-cursor)
  (send run-button enable #f)
  (set! break? #f)
  (let ((the-qix
        (let-values (((w h) (send canvas get-client-size)))
          (build-list
           n-qix
           (lambda (i)
             (make-qix (random w) (random h) (- (random 20) 10) (- (random 20) 10)
                       (random w) (random h) (- (random 20) 10) (- (random 20) 10)
                       (random-color) '()))))))
    (let loop ()
      (let ((t (current-milliseconds)))
        (draw-qix the-qix)
        (move-qix the-qix)
        (sleep/yield (max 0.0 (/ (- 10.0 (- (current-milliseconds) t)) 1000.0)))
        (unless break? (loop)))))
  (send run-button enable #t)
  (end-busy-cursor))

;; GUI elements

(define frame
  (instantiate frame% ("Animated Canvas Demo")))

(define panel
  (instantiate horizontal-panel% (frame)
    (alignment '(right center))
    (stretchable-height #f)))

(define run-button
  (instantiate button%
    ("Run" panel)
    (horiz-margin 4)
    (callback (lambda (b e)
                (main (send slider get-value))))))

(define stop
  (instantiate button%
    ("Stop" panel)
    (horiz-margin 4)
    (callback (lambda (b e)
                (set! break? #t)))))

(define slider
  (instantiate slider%
    ("Number of qix" 1 100 frame)
    (init-value 10)
    (style '(horizontal))))

(define canvas (instantiate animated-canvas% (frame)
                 (style '(border))
                 (min-width 800) (min-height 600)))

(send frame show #t)
