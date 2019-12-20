#lang racket/gui
;;; animated-canvas.rkt
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
;;; 1.0.0    12/28/07  Initial release. (MDW)
;;; 1.2.0    09/02/08  Updated for PLT Scheme 4.0. (MDW)
;;; 1.2.1    09/25/08  Added no-autoclear and transparent processing. (MDW)
;;; 1.2.2    09/25/08  Added resizing. (MDW)
;;; 2.0.0    06/01/10  Updated for Racket. (MDW)

;;; class animated-canvas%
;;; Implements a canvas that supports offscreen bitmaps for animation. Works
;;; like a standard canvas% object, but the device context points to the
;;; appropriate bitmap-dc% for drawing. A call to swap-bitmaps swaps the
;;; bitmaps and paits the old one.  [The device context is not valid across
;;; calls to swap-bitmap.]
(define animated-canvas%
  (class canvas%
    ;; Init parameters (shadows those from csnvas%)
    ;; so we can manipulate no-autoclear
    (init parent)
    (init (style '()))
    ;; Save animated-canvas no-autoclear and transparent style
    (define no-autoclear? (if (memq 'no-autoclear style) #t #f))
    (define transparent? (if (memq 'transparent style) #t #f))
    ;; Instantiate superclass with updated no-autoclear and transparent
    ;; arguments
    (set! style (if no-autoclear? style (cons 'no-autoclear style)))
    (set! style (remq 'transparent style))
    (super-instantiate (parent) (style style))
    ;; Inherit superclass methods
    (inherit get-client-size)
    (inherit refresh)
    ;; Create bitmaps
    (define bitmap-vector
      (let-values (((w h) (get-client-size)))
        (build-vector
         2
         (lambda (i)
           (make-object bitmap% w h)))))
    ;; Create from and to bitmap indices
    (define from-bitmap 0)
    (define to-bitmap 1)
    ;; Create from and to device contexts
    (define from-bitmap-dc (make-object bitmap-dc%))
    (define to-bitmap-dc (make-object bitmap-dc%))
    ;; Initialize from and to indices
    (send from-bitmap-dc set-bitmap (vector-ref bitmap-vector from-bitmap))
    (send to-bitmap-dc set-bitmap (vector-ref bitmap-vector to-bitmap))
    ;; Clear the from bitmap so the canvas initially is white.
    (send from-bitmap-dc clear)
    (send to-bitmap-dc clear)
    ;; Swap the bitmaps
    (define/public (swap-bitmaps)
      ;; Reset bitmap-dc instances
      (send from-bitmap-dc set-bitmap #f)
      (send to-bitmap-dc set-bitmap #f)
      ;; Swap bitmap indices
      (set! from-bitmap (modulo (+ from-bitmap 1) 2))
      (set! to-bitmap (modulo (+ to-bitmap 1) 2))
      ;; Set bitmap-dc instances
      (send from-bitmap-dc set-bitmap (vector-ref bitmap-vector from-bitmap))
      (send to-bitmap-dc set-bitmap (vector-ref bitmap-vector to-bitmap))
      ;; Check for client resize and make new bitmap if necessary
      (let-values (((w h) (get-client-size)))
        (let ((bitmap (send to-bitmap-dc get-bitmap)))
          (unless (and (= w (send bitmap get-width))
                       (= h (send bitmap get-height)))
            (vector-set! bitmap-vector to-bitmap (make-object bitmap% w h))
            (send to-bitmap-dc set-bitmap (vector-ref bitmap-vector to-bitmap))
            )))
      ;; Clear the to-bitmap
      (unless no-autoclear?
        (send to-bitmap-dc clear))
      ;; Refresh the canvas
      (refresh)
      (yield))
    ;; Override the superclass get-dc method to return the bitmap-dc of the
    ;; to bitmap.
    (define/override-final (get-dc)
      to-bitmap-dc)
    ;; Override the superclass on-paint method to move the from-bitmap to the
    ;; canvas.
    (define/override-final (on-paint)
      (let ((canvas-dc (super get-dc)))
        (send canvas-dc draw-bitmap
              (vector-ref bitmap-vector from-bitmap) 0 0)))))

;;; Module Contracts

(provide animated-canvas%)
