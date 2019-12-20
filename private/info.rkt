#lang setup/infotab
(define name "Animated Canvas")
(define blurb
  (list "This library provides an animated-canvas% class that specializes the GRacket "
        "canvas% class to provide a simple double-buffered animation capability in Racket. "
        "Simple demonstration programs are also provided, including two that show how to "
        "animate plots using the new plot collection."))
(define release-notes
  (list "Added animations of insertion sort and bubble sort using the plot collection."))
(define primary-file "animated-canvas.rkt")
(define categories '(ui))
(define repositories '("4.x"))
(define scribblings '(("animated-canvas.scrbl" ())))
(define required-core-version "5.2")