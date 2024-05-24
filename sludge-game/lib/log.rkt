#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require racket/class
         racket/string
         (prefix-in gui: racket/gui)
         (only-in racket/gui/easy/private/observable ->obs)
         racket/gui/easy
         racket/gui/easy/operator)

(provide editor-canvas log-text% input-text% autocomplete-text%)

#|
window
┣━(3d viewer ...)
┗━ vpanel
   ┣━ editor-canvas canvas<%> (for the log)
   ┃  ┗━ log-text text% editor<%>
   ┃     ┗━(log line snips ...)
   ┗━ editor-canvas canvas<%> (for the log)
      ┗━ input-text text% editor<%>
         ┣━(editable snips ...)
         ┗━ editor-snip% (for autocomplete)
            ┗━ log-autocomplete text% editor<%>
               ┗━ (autocomplete snips ...)
|#

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
           [style (if editable? '(no-hscroll no-vscroll) '(no-hscroll no-focus))]))

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
    (define/public (show-intro)
      (send this insert "SLUDGE FICTION\nAN INTERACTIVE STORY\nWHERE YOUR CHOICES DON'T MATTER")
      (send this insert "\n"))

    (define/public (add snip)
      (send this insert "\n" (send this last-position) 'same)
      (send this insert snip (send this last-position) 'same))))

(define input-text%
  (class gui:text%
    (init-field return-cb tab-cb @input [unlocked? (make-parameter #t)])
    (inherit last-position find-snip get-snip-position set-position delete get-text insert)
    (define (get-text-end-position)
      (define ac (find-snip (last-position) 'after))
      (get-snip-position ac))
    (define (get-input)
      (get-text 1 (get-text-end-position)))
    (define/public (delete-input)
      (delete 1 (get-text-end-position)))
    (define/public (insert-input input)
      (insert input (get-text-end-position)))
    (define/public (apply-constraints)
      (unlocked? #f)
      (set-position (get-text-end-position) 'same))
    (define/augment (can-insert? s l)
      (or (unlocked?) (and (s . >= . 1) ; don't allow deleting the > symbol
                           (= s (get-text-end-position)))))
    (define/augment (can-delete? s l)
      (or (unlocked?) (and (s . >= . 1)
                           (= (+ s l) (get-text-end-position)))))
    (define/override (on-default-char ev)
      (define code (send ev get-key-code))
      (cond
        [(eq? code #\return) (return-cb (get-input))]
        [(eq? code #\tab) (tab-cb)]
        [(and (send ev get-control-down) (eq? code #\backspace)) (delete-input)]
        [(send ev get-control-down) #f]
        [else (super on-default-char ev)]))
    (define/override (on-event ev)
      (when (or (send ev get-left-down) (send ev get-middle-down) (send ev get-right-down))
        (set-position (get-text-end-position) 'same)))
    (define/augment (after-insert s l)
      (:= @input (get-input)))
    (define/augment (after-delete s l)
      (:= @input (get-input)))
    (super-new)))

(define autocomplete-text%
  (class gui:text%
    (define/override (on-default-char ev) #f)
    (super-new)))

