#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(define angle 45)
(define height 80.20)
(define spacing 160)

(define (square-pyramid)
  (intersection
   (at `[0 ,(- (/ height 2)) 0]
       (scale '[1 0.5 1]
              (cube (* height 2))))
   (rotate angle #:around 'z
           (half-space '[0 1 0] 0))
   (rotate (- angle) #:around 'z
           (half-space '[0 1 0] 0))
   (rotate angle #:around 'x
           (half-space '[0 1 0] 0))
   (rotate (- angle) #:around 'x
           (half-space '[0 1 0] 0))))

(at
 '[-150 0 0]
 (rotate 30 #:around 'y
         (at `[0 ,spacing 0]
             (square-pyramid))

         (rotate 90 #:around 'z
                 (at `[0 ,spacing 0]
                     (square-pyramid)))

         (rotate -90 #:around 'z
                 (at `[0 ,spacing 0]
                     (square-pyramid)))

         (rotate -90 #:around 'x
                 (at `[0 ,spacing 0]
                     (square-pyramid)))

         (rotate 90 #:around 'x
                 (at `[0 ,spacing 0]
                     (square-pyramid)))

         (rotate 180 #:around 'x
                 (at `[0 ,spacing 0]
                     (square-pyramid)))))

(at
 '[150 0 0]
 (rotate -20 #:around 'y
         (iso -5
              (difference
               (union
                (difference
                 (scale '[0.7 1 1.02]
                        (sphere 147))
                 (half-space '[0 1 0] 0))
                (capsule 100 100))
               (at '[100 150 0]
                   (scale '[0.5 1 1]
                          (sphere 175)))
               (at '[-100 150 0]
                   (scale '[0.5 1 1]
                          (sphere 175)))))))
