#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require racket/gui/easy
         racket/gui/easy/operator)
(provide (all-defined-out))

(define/obs @flags
  (hasheq 'a #f
          'b #t
          'closet:taken 'nothing))

(define (toggle-flag k)
  (@flags . <~ . (λ (flags) (hash-update flags k (λ (v) (not v))))))

(define (get-flag k)
  (hash-ref (obs-peek @flags) k))

(define (flag-eq? k v)
  (eq? (hash-ref (obs-peek @flags) k) v))

(define (set-flag k v)
  (@flags . <~ . (λ (flags) (hash-set flags k v))))

(define (interactive-set-flag k)
  (define/obs @v "")
  (define (ok . _)
    (set-flag k (string->symbol (obs-peek @v)))
    (renderer-destroy d))
  (define d
    (render
     (window
      #:size '(300 #f)
      #:title (format "Set flag ~a" k)
      (input @v (λ (action text)
                  (:= @v text)
                  (when (eq? action 'return)
                    (ok))))
      (button "OK" ok))))
  (void))
