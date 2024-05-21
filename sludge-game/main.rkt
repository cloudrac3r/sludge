#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define debug-mode #t)
(define-for-syntax enable-designs #f)

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/

(define-syntax (when-design stx)
  (syntax-parse stx
    [(_ body (else e))
     (if enable-designs
         #'body
         #'e)]
    [(_ body)
     #'(when-design body (else (begin)))]
    [(_ body ...)
     #'(begin (when-design body) ...)]))

(when-design
 (require "lib/wayland.rkt"))

(require racket/class
         racket/contract
         racket/format
         racket/function
         racket/match
         racket/set
         racket/string
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         "lib/at-map.rkt"
         "lib/flags.rkt"
         "lib/world.rkt"
         "lib/log.rkt"
         "room.rkt")

(when-design
 (require (prefix-in design: "design/all.rkt")
          "../viz/spheretrace-viewer.rkt"))

;; --- MODELS ----------------------------------------------------------------------------------------

(when-design
 (define models (hash->list design:all #t))
 (define/obs @design-thunk (cdr (car models))))

;; --- ROOMS -----------------------------------------------------------------------------------------

(define/obs @visited (set))
(define/obs @current-room-id 'room:bedroom)
(define @current-room
  (@current-room-id . ~> . (λ (id) (hash-ref world id))))

(define (execute-room id)
  (:= @current-room-id id)
  (define room (obs-peek @current-room))
  (if (set-member? (obs-peek @visited) id)
      (for ([id (room-re-entry-cutscene room)])
        (execute-cutscene id))
      (for ([id (append (room-entry-cutscene room) (room-description room))])
        (execute-cutscene id)))
  (@visited . <~ . (λ (visited) (set-add visited id))))

;; --- CUTSCENES -------------------------------------------------------------------------------------

(define/obs @interaction #f)

(define (execute-cutscene id)
  (define cutscene (hash-ref world id))
  (define next ((cutscene-def cutscene)))
  (let loop ()
    (define i (next))
    (when
        (match i
          [(? void?)
           #f #| break out of cutscene |#]

          [(? string?)
           (add-to-log i)]

          [(list (? (curryr is-a? view<%>) view) (? semaphore? sem))
           (:= @interaction view)
           (thread (λ () (semaphore-wait sem) (loop)))
           #f #| only continue cutscene in thread |#]

          [else
           (error 'execute-cutscene "cutscene ~v produced a value ~v which did not match any patterns" id i)])

      (:= @interaction #f)
      (loop))))

;; --- LOG -------------------------------------------------------------------------------------------

(define log (new log-text%))
(define (add-to-log s)
  (when debug-mode
    (printf "adding to log: ~v~n" s))
  (send log add (make-object gui:string-snip% s)))

(define/obs @input "")

(define (clear)
  (:= @input ""))

(define/obs @autocomplete '("one" "two" "three"))
(define @matching-autocomplete
  (@> (for/list ([ac @autocomplete]
                 #:when (string-prefix? ac @input))
        ac)))

;; --- GAME LOOP -------------------------------------------------------------------------------------

(define (process-input text)
  (add-to-log "")
  (add-to-log (format ">~a" text))
  (define words (string-split text))
  (define room (obs-peek @current-room))
  (match words
    [(list "go")
     (add-to-log "Where do you want to go?")]

    [(list "go" dest)
     (define dest-sym (string->symbol (format "room:~a" dest)))
     (cond
       [(eq? dest-sym (room-id room))
        (add-to-log "You're already there.")]
       [(memq dest-sym (room-go room))
        (execute-room dest-sym)]
       [else
        (add-to-log "Sorry, I don't know where that is.")])]

    [(list "look")
     (for ([id (room-description room)])
       (execute-cutscene id))]

    [(list "look" thing)
     (define thing-sym (string->symbol (format "cutscene:~a" thing)))
     (if (memq thing-sym (room-look room))
         (execute-cutscene thing-sym)
         (add-to-log "Sorry, I don't know what that is."))]

    [(list verb _ ...)
     (add-to-log (format "Sorry, I don't know what '~a' means." verb))])

  (clear))

(void (execute-room (obs-peek @current-room-id)))

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
                                      #:checked? (@> (eq? id @current-room-id)))))
        (apply menu "Cutscene"
               (for/list ([(id cutscene) (in-hash world)]
                          #:when (cutscene? cutscene))
                 (menu-item (~a id) (λ _ (execute-cutscene id)))))
        (when-design
         (apply menu "Model"
                (for/list ([model models])
                  (menu-item (~a (car model)) (λ _ (:= @design-thunk (cdr model))))))
         (else (menu "Model"
                     (menu-item #:enabled? #f "<models not loaded>"))))
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
    (when-design
     (vpanel
      #:min-size (@> (list (truncate (* @width 1/3)) #f))
      (spheretrace-viewer
       @design-thunk)
      (text "Drag to rotate, scroll to zoom, Z to reset."))
     (else (vpanel*)))
    (vpanel
     (editor-canvas log)
     (vpanel-
      (observable-view
       @interaction
       (λ (view)
         (or view
             (vpanel
              (hpanel-
               (input #:label "Your next move: " @input
                      (λ (action text)
                        (:= @input text)
                        (when (eq? action 'return)
                          (process-input text)))))
              (hpanel-
               (list-view
                @matching-autocomplete
                #:style '(horizontal)
                (λ (k _)
                  (text k)))
               (text ""))))))))))))
