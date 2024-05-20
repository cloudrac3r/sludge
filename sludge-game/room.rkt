#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require racket/generator
         racket/gui/easy
         racket/gui/easy/operator
         "lib/flags.rkt"
         "lib/world.rkt")
(provide (all-defined-out))



(define-room bedroom
  #:entry-cutscene 'cutscene:bedroom
  #:re-entry-cutscene 'cutscene:bedroom-re-entry
  #:go '(room:closet)
  #:look null
  #:commands null)



(define-cutscene (bedroom)
  (yield "The bedroom is in its usual untidy state."))



(define-cutscene (bedroom-re-entry)
  (yield "You are in the bedroom."))



(define-room closet
  #:entry-cutscene 'cutscene:closet
  #:re-entry-cutscene #f
  #:go '(room:bedroom)
  #:look '(cutscene:suit cutscene:sludge-merch)
  #:commands
  (list (command "wear" "suit" 'cutscene:wear-suit)
        (command "wear" "sludge merch" 'cutscene:wear-sludge-merch)))



(define-cutscene (closet)
  (yield
   (case (get-flag 'closet:taken)
     [(nothing) "You step between the open doors of the closet. The bedroom light has never been able to illuminate its depths. The floor is piled with dirty clothes that are waiting for a sunny day, but you're able to identify two quite presentable outfits hanging on the rail. One of them is a formal blue suit you've been saving for a job interview. The other is a branded Sludge Co t-shirt you won in a raffle."]
     [(suit) "The Sludge Co t-shirt looks mockingly at you from the end of the rail. You try to push it out of your mind."]
     [(merch) "The suit remains on the rail dispassionately."])))



(define-cutscene (wear-suit)
  (set-flag 'closet:taken 'suit)
  (yield "You put on the suit."))



(define-cutscene (wear-merch)
  (set-flag 'closet:taken 'merch)
  (yield "You pull the hideous t-shirt over your torso. You also put on some shorts lying nearby. The colours almost don't clash."))
