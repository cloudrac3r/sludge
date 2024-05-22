#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


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
                 (sphere 175)))))
