#lang ruckus

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(define (frond ang height col)
  (color col
         (at (list -50 height 0)
             (rotate ang #:around 'y
                     (rotate 90 #:around 'x
                             (extrude 120 (circle 3)))))))

(define bowl-angle 70)
(define bowl-height 150)
(rotate 180 #:around 'x
        (intersection
         (at `[0 ,(- (/ bowl-height 2)) 0]
             (scale '[1 0.5 1]
                    (cube (* bowl-height 2))))
         (rotate bowl-angle #:around 'z
                 (half-space '[0 1 0] 0))
         (rotate (- bowl-angle) #:around 'z
                 (half-space '[0 1 0] 0))
         (rotate bowl-angle #:around 'x
                 (half-space '[0 1 0] 0))
         (rotate (- bowl-angle) #:around 'x
                 (half-space '[0 1 0] 0))
         (half-space '[0 1 0] -80)))
(frond 55 180 '[0.5 0.3 0.04])
(frond 55 190 '[0.5 0.3 0.04])
