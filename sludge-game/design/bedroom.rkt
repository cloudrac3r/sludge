#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(scale
 3
 ;; duvet
 (at '[-30 10 0]
     (scale '[0.65 0.3 0.5]
            (iso 50
                 (cube 200))))

 ;; bed
 (color '[0.2 0.2 0.2]
        (at '[0 0 0]
            (rotate 90 #:around 'z
                    (rotate 90 #:around 'x
                            (extrude 75
                                     (rect 120 240))))))

 ;; pillow
 (at '[95 46 0]
     (color '[0.7 0.7 0.7]
            (scale '[0.95 0.3 1.4]
                   (iso 22
                        (cube 20))))))
