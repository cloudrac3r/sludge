#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define debug-mode #t)
(define-for-syntax enable-designs #t)

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
         racket/list
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

;; --- ROOMS -----------------------------------------------------------------------------------------

(define/obs @visited (set))
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

;; --- MODELS ----------------------------------------------------------------------------------------

(when-design
 (define models (hash->list design:all #t))
 (define/obs @design-thunk design:empty)
 (obs-observe!
  @current-room
  (λ (room)
    (:= @design-thunk (hash-ref design:all (or (room-model room) 'empty))))))

;; --- CUTSCENES -------------------------------------------------------------------------------------

(define/obs @interaction #f)

(define (execute-cutscene id)
  (define cutscene (hash-ref world id))
  (define next ((cutscene-def cutscene)))
  (let loop ()
    (:= @interaction #f)
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

          [(list "go" dest-sym)
           (execute-room (:= @current-room-id dest-sym))
           #f #| switch to the cutscene of the new room |#]

          [else
           (error 'execute-cutscene "cutscene ~v produced a value ~v which did not match any patterns" id i)])

      (loop))))

;; --- LOG -------------------------------------------------------------------------------------------

(define @debug-mode (obs debug-mode))

(define log (new log-text%))
(send log show-intro)
(define (add-to-log s)
  (when (obs-peek @debug-mode)
    (printf "adding to log: ~v~n" s))
  (define snip (if (is-a? s gui:snip%)
                   s
                   (make-object gui:string-snip% s)))
  (send log add snip))

(define (show-how-to-play)
  (add-to-log "HOW TO PLAY THE GAME")
  (add-to-log "Click the input box at the bottom. Write what you want to do, then press Return.")
  (add-to-log "The blue text suggests your available actions. Press Tab to accept suggestion.")
  (add-to-log "Feedback will appear in this top area.")
  (add-to-log ""))
(show-how-to-play)

(define/obs @input "")
(define input-text (new input-text%
                        [return-cb (λ (text) (process-input (string-trim text)))]
                        [tab-cb (λ () (autocomplete-right))]
                        [@input @input]))

;; To avoid letters clipping on the editor-snip:
;; Windows: 125%, 150%, 175% zoom needs 1 spacing. 100%, 200% zoom needs 0 spacing.
(define left-margin (if (integer? (gui:get-display-backing-scale)) 0 1))
(define ac-editor (new autocomplete-text%))
(define ac-editor-snip (new gui:editor-snip%
                            [editor ac-editor]
                            [with-border? #f]
                            [top-margin 0]
                            [right-margin 0]
                            [bottom-margin 0]
                            [left-margin left-margin]
                            [left-inset 0]))
(send input-text insert ">")
(send input-text insert ac-editor-snip)

(define sl (send ac-editor get-style-list))
(define standard (send sl find-named-style "Standard"))
(define ac-style (send sl new-named-style "Autocomplete" standard))
(send ac-style set-delta
      (send (make-object gui:style-delta%)
            set-delta-foreground "blue"))
(define ac-ul-style (send sl new-named-style "Autocomplete Underline" standard))
(send ac-ul-style set-delta
      (send (make-object gui:style-delta% 'change-toggle-underline)
            set-delta-foreground "blue"))
(define cmd-style (send sl new-named-style "Command" standard))
(send cmd-style set-delta
      (send (make-object gui:style-delta%)
            set-delta-foreground "blue"))

(send input-text apply-constraints)

(define (split-command str)
  (cdr (regexp-match #rx"([^ ]*) *(.*)" str)))

(define @input-verb (@> (car (split-command @input))))
(define @input-object (@> (cadr (split-command @input))))

(define (clear)
  (send input-text delete-input))

(define @autocomplete-verb
  (@> (for/set ([command (in-list (append '(("look") ("go")) ; always suggest completing these verbs
                                          (hash-keys (room-commands @current-room) #t)))]
                #:when (string-prefix? (car command) @input-verb))
        (car command))))

(define @autocomplete-verb-trimmed
  (@> (for/list ([str (in-set @autocomplete-verb)]
                 [i (in-naturals)])
        (if (= i 0)
            (substring str (string-length (obs-peek @input-verb)))
            str))))

(define @autocomplete-object
  (@> (for/list ([command (in-list (hash-keys (room-commands @current-room) #t))]
                 #:when (and (equal? (car command) @input-verb)            ; verb equals input
                             ((length command) . >= . 2)                   ; command has an object
                             (string-prefix? (cadr command) @input-object) ; object matches input
                             ))
        (cadr command))))

(define @autocomplete-object-trimmed
  (@> (for/list ([str @autocomplete-object]
                 [i (in-naturals)])
        (if (= i 0)
            (substring str (string-length (obs-peek @input-object)))
            str))))

(define @autocomplete
  (@> (cond
        [(pair? @autocomplete-object) (obs-peek @autocomplete-object-trimmed)]
        [(not (string-contains? (obs-peek @input) " ")) (obs-peek @autocomplete-verb-trimmed)]
        [else null])))

(define (autocomplete-right)
  (define input (obs-peek @input))
  (define ac (obs-peek @autocomplete))
  (when (pair? ac)
    (define spacer (if (or (null? (obs-peek @autocomplete-object))
                           (string-contains? input " ")) "" " "))
    (send input-text insert-input (string-append spacer (car ac) " "))))

(obs-observe! @autocomplete
              (λ (v)
                (define input (obs-peek @input))
                (define spacer (if (or (null? (obs-peek @autocomplete-object))
                                       (string-contains? input " ")) "" " "))
                (send ac-editor begin-edit-sequence)
                (send ac-editor erase)
                (send ac-editor insert spacer)
                (for ([s v]
                      [i (in-naturals)])
                  (define snip (make-object gui:string-snip% s))
                  (send snip set-style ac-ul-style)
                  (send ac-editor insert snip)
                  (when (i . < . (sub1 (length v)))
                    (define sep (make-object gui:string-snip% " | "))
                    (send sep set-style ac-style)
                    (send ac-editor insert sep)))
                (send ac-editor end-edit-sequence)))

;; --- GAME LOOP -------------------------------------------------------------------------------------

(define (execute sym-or-obj)
  (define obj
    (if (symbol? sym-or-obj)
        (hash-ref world sym-or-obj)
        sym-or-obj))
  (if (room? obj)
      (execute-room (room-cutscene-id obj))
      (execute-cutscene (room-cutscene-id obj))))

(define (process-input text)
  (add-to-log "")
  (define cmd-line (make-object gui:string-snip% (format ">~a" text)))
  (send cmd-line set-style cmd-style)
  (add-to-log cmd-line)
  (define words (split-command text))
  (define room (obs-peek @current-room))
  (define valid-verbs (list->set (append '("look" "go")
                                         (for/list ([k (hash-keys (room-commands room))])
                                           (car k)))))
  (match words
    [(list "" "")
     (show-how-to-play)]

    [(list "debug" "")
     (@debug-mode . <~ . not)
     (add-to-log (format "Debug menus are now ~a." (if (obs-peek @debug-mode) "ENABLED" "DISABLED")))]

    [(list "go" "")
     (add-to-log (format "Where do you want to go? (Possibilities: ~a)"
                         (string-join (obs-peek @autocomplete-object) ", ")))]

    [(list "look" "")
     (for ([id (room-description room)])
       (execute-cutscene id))]

    [(? (curry hash-has-key? (room-commands room)))
     (define dest (hash-ref (room-commands room) words))
     (execute dest)]

    [(list (? (curry set-member? valid-verbs)) object)
     (add-to-log (format "Sorry, '~a' is not accessible from this room." object))]

    [(list verb _ ...)
     (add-to-log (format "Sorry, I don't know what '~a' means." verb))])

  (clear))

(void (execute-room (obs-peek @current-room-id)))

;; --- INTERFACE -------------------------------------------------------------------------------------

(define/obs @width 1000)

(define (make-window)
  (render
   (window
    #:size (list (obs-peek @width) 600)
    #:title "Sludge"
    #:mixin (λ (%)
              (class %
                (super-new)
                (define/override (on-size width height)
                  (:= @width width))))
    (if (obs-peek @debug-mode)
        (menu-bar
         (apply menu "Room"
                (for/list ([(id room) (in-hash world)]
                           #:when (room? room))
                  (checkable-menu-item (room-name room) (λ _ (execute-room id))
                                       #:checked? (@> (eq? id @current-room-id)))))
         (apply menu "Cutscene"
                (let ([next-levels
                       (for/list ([(id cutscene) (in-hash world)]
                                  #:when (cutscene? cutscene))
                         (car (string-split (cutscene-name cutscene) " ")))])
                  (for/list ([l (remove-duplicates (sort next-levels string<?))])
                    (apply menu l
                           (for/list ([(id cutscene) (in-hash world)]
                                      #:when (cutscene? cutscene)
                                      #:when (string-prefix? (cutscene-name cutscene) l))
                             (menu-item (~a id) (λ _ (execute-cutscene id))))))))
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
       (spheretrace-viewer
        @design-thunk)
       (text "Drag to rotate, scroll to zoom, Z to reset."))
      (else (vpanel*)))
     (vpanel
      #:min-size (@> (list (truncate (* @width 1/6)) #f))
      (editor-canvas log #f)
      (vpanel-
       (observable-view
        @interaction
        (λ (view)
          (or view
              (vpanel
               (hpanel-
                #:min-size '(#f 38)
                (editor-canvas input-text #t))))))))))))

(define r (make-window))
(obs-observe! @debug-mode (λ _
                            (renderer-destroy r)
                            (set! r (make-window))))
