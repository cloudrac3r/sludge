#lang racket/base

(define debug-mode #t)

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/

(require "lib/wayland.rkt")

(require racket/class
         racket/contract
         racket/format
         racket/match
         racket/set
         racket/string
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         "../viz/spheretrace-viewer.rkt"
         "lib/at-map.rkt"
         "lib/flags.rkt"
         "lib/world.rkt"
         "lib/log.rkt"
         (prefix-in design: "design/all.rkt")
         "room.rkt")

;; --- MODELS ----------------------------------------------------------------------------------------

(define models (hash->list design:all #t))
(define/obs @design-thunk (cdr (car models)))

;; --- ROOMS -----------------------------------------------------------------------------------------

(define/obs @visited (set))
(define/obs @current-room 'room:bedroom)

(define (execute-room id)
  (:= @current-room id)
  (define room (hash-ref world id))
  (if (set-member? (obs-peek @visited) id)
      (execute-cutscene (room-re-entry-cutscene room))
      (execute-cutscene (room-entry-cutscene room)))
  (@visited . <~ . (λ (visited) (set-add visited id))))

;; --- CUTSCENES -------------------------------------------------------------------------------------

(define (execute-cutscene id)
  (when id
    (define cutscene (hash-ref world id))
    (for ([i (in-producer ((cutscene-def cutscene)) (void))])
      (add-to-log i))))

;; --- LOG -------------------------------------------------------------------------------------------

(define log (new log-text%))
(define (add-to-log s)
  (when debug-mode
    (printf "adding to log: ~v~n" s))
  (send log add (make-object gui:string-snip% s)))

(define/obs @input "")

(define (clear)
  (:= @input ""))

;; --- GAME LOOP -------------------------------------------------------------------------------------

(define (process-input text)
  (add-to-log "")
  (add-to-log (format ">~a" text))
  (define words (string-split text))
  (match words
    [(list "go")
     (add-to-log "Where do you want to go?")]
    [(list "go" dest)
     (execute-room (string->symbol dest))]
    [_
     (add-to-log "Sorry, I don't know what that means.")])
  (clear))

(void (execute-room (obs-peek @current-room)))

;; --- INTERFACE -------------------------------------------------------------------------------------

(define/obs @width 1000)

(void
 (render
  (window
   #:size (list (obs-peek @width) 600)
   #:title "Sludge"
   #:mixin (λ (%)
             (class %
               (super-new)
               (define/override (on-size width height)
                 (:= @width width))))
   (if debug-mode
       (menu-bar
        (apply menu "Room"
               (for/list ([(id room) (in-hash world)]
                          #:when (room? room))
                 (checkable-menu-item (room-name room) (λ _ (execute-room id))
                                      #:checked? (@> (eq? id @current-room)))))
        (apply menu "Cutscene"
               (for/list ([(id cutscene) (in-hash world)]
                          #:when (cutscene? cutscene))
                 (menu-item (~a id) (λ _ (execute-cutscene id)))))
        (apply menu "Model"
               (for/list ([model models])
                 (menu-item (~a (car model)) (λ _ (:= @design-thunk (cdr model))))))
        (apply menu "Flags"
               (let ([flags (obs-peek @flags)])
                 (for/list ([k (hash-keys (obs-peek @flags) #t)])
                   (define name (symbol->string k))
                   (define v (hash-ref flags k))
                   (define @v (@> (hash-ref @flags k)))
                   (if (boolean? v)
                       (checkable-menu-item
                        name
                        #:checked? @v
                        (λ _ (toggle-flag k)))
                       (menu-item
                        (@> "~a = ~a" name @v)
                        (λ _ (interactive-set-flag k))))))))
       (hpanel-))
   (hpanel
    (vpanel
     #:min-size (@> (list (truncate (* @width 1/3)) #f))
     (spheretrace-viewer
      @design-thunk)
     (text "Drag to rotate, scroll to zoom, Z to reset."))
    (vpanel
     (editor-canvas log)
     (input #:label "Your next move: " @input
            (λ (action text)
              (when (eq? action 'return)
                (process-input text)))))))))
