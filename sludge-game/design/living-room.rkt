#lang reader "reader.rkt"

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(define (frond ang fac-ang height len center-dist col)
  (color col
         (rotate fac-ang #:around 'y
                 (at (list center-dist height 0)
                     (rotate ang #:around 'y
                             (rotate 90 #:around 'x
                                     (extrude len (circle 6))))))))
(define pot-angle 70)
(define pot-height 150)

;; bookshelf
(at '[-200 100 -250]
    (difference
     (scale '[2 3 1]
            (cube 200))
     (color '[0.3 0.3 0.3]
            (at '[0 0 140]
                (scale '[1.8 0.7 0.9]
                       (cube 200))
                (at '[0 170 0]
                    (scale '[1.8 0.7 0.9]
                           (cube 200)))
                (at '[0 -170 0]
                    (scale '[1.8 0.7 0.9]
                           (cube 200))))))

    (at '[-80 220 0]
        (rotate 180 #:around 'x
                (intersection
                 (at `[0 ,(- (/ pot-height 2)) 0]
                     (scale '[1 0.5 1]
                            (cube (* pot-height 2))))
                 (rotate pot-angle #:around 'z
                         (half-space '[0 1 0] 0))
                 (rotate (- pot-angle) #:around 'z
                         (half-space '[0 1 0] 0))
                 (rotate pot-angle #:around 'x
                         (half-space '[0 1 0] 0))
                 (rotate (- pot-angle) #:around 'x
                         (half-space '[0 1 0] 0))
                 (half-space '[0 1 0] -80)))
        (for ([i (in-range 0 360 40)]
              [c (in-cycle (in-list '([0.3 0.8 0.08]
                                      [0.1 0.85 0.08]
                                      [0.2 0.7 0.08])))]
              [a (in-cycle (in-list '(53 55 60)))]
              [l (in-cycle (in-list '(120 128 150 135 140)))])
          (frond a i 180 l -50 c))
        (for ([i (in-range 0 360 72)]
              [c (in-cycle (in-list '([0.3 0.8 0.08]
                                      [0.1 0.85 0.08]
                                      [0.2 0.7 0.08])))]
              [a (in-cycle (in-list '(28 35 22)))]
              [l (in-cycle (in-list '(130 140 120 125)))])
          (frond a i 220 l -30 c))))

(define (dec x z w h r c)
  (at (list x 0 z)
      (rotate r #:around 'z
              (rotate 90 #:around 'x
                      (extrude 2
                               (color c
                                      (rect w h)))))))

;; table
(at '[100 -200 250]
    (scale
     2.5
     (difference
      (union
       (difference
        (cube 200)
        (sphere 120))
       (difference
        (cube 200)
        (half-space '[0 1 0] 72)))
      (half-space '[0 1 0] 0)))
    ;; laptop
    (color '[0 0 0]
           (at '[-130 255 -160]
               (rotate 78 #:around 'z
                       (rotate 90 #:around 'x
                               (extrude 10
                                        (rect (* 1.2 160) (* 1.2 90)))))))
    ;; decorations
    (at '[-30 255 20]
        (for ([x '(50 40 70 10 20 70 90 60 10 89 29 49)]
              [y '(50 30 20 70 50 30 83 75 12 38 23 10)]
              [s (in-cycle (in-list '(1 0.9 1.1 1.25 1.6)))]
              [c (in-cycle (in-list '([1 0 0]
                                      [1 1 0]
                                      [0 0.6 0]
                                      [0 0.2 1]
                                      [1 0 1])))])
          (dec (* 2 x) (* 2 y) (* s 32) (* s 18) 0 c))))
