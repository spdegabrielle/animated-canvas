#lang info
(define collection "animated-canvas")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/animated-canvas.scrbl" ())))
(define pkg-desc "This library provides an animated-canvas% class that specializes the racket/gui canvas% class to provide a simple double-buffered animation capability in Racket. Simple demonstration programs are also provided, including two that show how to animate plots using the new plot collection.")
(define version "0.1")
(define pkg-authors '("Doug Williams m.douglas.williams@gmail.com"))
(define release-notes
  (list "Added animations of insertion sort and bubble sort using the plot collection."))
