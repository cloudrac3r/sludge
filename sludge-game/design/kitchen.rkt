#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(define (stool)
      (at '[0 10 0]
          (rotate 90 #:around 'x
                  (extrude 20 (circle 20))))
      (at '[0 -10 0]
          (rotate 90 #:around 'x
                  (extrude 40 (circle 5)))))

;; center the whole thing
(scale 3
       (at '[0 0 -15]
           (difference
            ;; counter
            (union
             (at '[20 0 -100]
                 (scale '[1 0.3 0.3]
                        (cube 200)))
             (at '[90 0 40]
                 (scale '[0.3 0.3 1.5]
                        (cube 200))))
            ;; sink basin
            (at '[87 25 30]
                (color '[0.3 0.3 0.3]
                       (scale '[1 1 1.5]
                              (cube 40)))))
           ;; tap
           (at '[100 40 30]
               (rotate 50 #:around 'y
                       (rotate 90 #:around 'x
                               (extrude 40 (circle 2)))))

           (at '[20 0 120] (stool))
           (at '[20 0 170] (stool))))
