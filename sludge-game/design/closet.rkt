#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


;; center the whole thing
(scale
 4.5

 ;; closet
 (difference
  (scale '[0.8 1 0.6]
         (cube 200))
  (at '[0 0 40]
      (scale '[0.7 0.8 0.6]
             (color '[0.2 0.2 0.2]
                    (cube 200)))))

 ;; junk
 (color '[0.35 0.35 0.35]
        (smooth-union 8
                      (at '[0 -70 10]
                          (sphere 30))
                      (at '[24 -70 8]
                          (sphere 20))
                      (at '[20 -65 10]
                          (sphere 18))
                      (at '[-50 -70 10]
                          (sphere 22))
                      (at '[-35 -70 26]
                          (sphere 17))
                      (at '[-35 -60 0]
                          (sphere 15)))
        (at '[52 -70 10]
            (cube 30)))

 ;; rail
 (at '[0 48 20]
     (rotate 90 #:around 'y
             (extrude 140 (circle 3))))

 ;; hanging sludge merch
 (at '[60 22 20]
     (iso 2
          ;; main
          (color '[0.25 0.2 0]
                 (rotate 90 #:around 'y
                         (extrude 1
                                  (difference
                                   (rotate 45
                                           (rect 40 40))
                                   (at '[0 -30]
                                       (rect 50 50))
                                   (at '[0 45]
                                       (rect 50 50)))
                                  (at '[0 -20]
                                      (rect 30 40)))))
          ;; design
          (at '[-0.3 -3 0.5]
              (color '[1 0.05 0.05]
                     (rotate 90 #:around 'y
                             (extrude 1
                                      (circle 9)))))
          (at '[-0.4 -9 2]
              (color '[1 1 0.05]
                     (rotate 90 #:around 'y
                             (extrude 1
                                      (circle 3)))))
          (at '[-0.5 -4 0]
              (color '[0 0.5 0]
                     (rotate 90 #:around 'y
                             (extrude 1
                                      (rotate -15
                                              (rect 25 2))))))))

 ;; hanging suit
 (at '[35 22 20]
     (iso 2
          ;; collar
          (color '[1 1 1]
                 (rotate 90 #:around 'y
                         (at '[0 0 0.5]
                             (extrude 1
                                      (at '[0 9]
                                          (rotate 45
                                                  (rect 15 15)))))))
          ;; main
          (color '[0 0.02 0.6]
                 (rotate 90 #:around 'y
                         (extrude 1
                                  (difference
                                   (rotate 45
                                           (rect 40 40))
                                   (at '[0 -30]
                                       (rect 50 50))
                                   (at '[0 45]
                                       (rect 50 50)))
                                  (at '[0 -18]
                                      (rect 57 36))
                                  (at '[0 -36]
                                      (rect 40 36))))))))
