#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require racket/class
         (prefix-in gui: racket/gui)
         (only-in racket/gui/easy/private/observable ->obs)
         racket/gui/easy
         racket/gui/easy/operator)

(provide editor-canvas log-text% log-input%)

(define editor-canvas%
  (class* object% (view<%>)
    (init-field @editor editable?)
    (super-new)

    (define/public (dependencies)
      (list @editor))

    (define/public (create parent)
      (new gui:editor-canvas%
           [parent parent]
           [editor (obs-peek @editor)]
           [style (if editable? '(no-hscroll) '(no-hscroll no-focus))]))

    (define/public (update v what val)
      (case/dep what
        [@editor
         (send v set-editor val)]))

    (define/public (destroy v)
      (void))))

(define (editor-canvas @editor editable?)
  (new editor-canvas%
       [@editor (->obs @editor)]
       [editable? editable?]))

(define log-text%
  (class gui:text%
    (super-new)
    (send this auto-wrap #t)
    (send this insert "SLUDGE FICTION\nAN INTERACTIVE STORY\nWHERE YOUR CHOICES DON'T MATTER")

    (define/public (add snip)
      (send this insert "\n" (send this last-position) 'same)
      (send this insert snip (send this last-position) 'same))))

(define log-input%
  (class gui:text%
    (inherit last-position)
    (define/augment (can-insert? s l) (= s (last-position)))
    (define/augment (can-delete? s l) (= (+ s l) (last-position)))
    (define/override (on-default-char ev)
      (println (send ev get-key-code)))
    (super-new)))
