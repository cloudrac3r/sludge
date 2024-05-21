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
  #:description '(cutscene:bedroom-description)
  #:re-entry-cutscene '(cutscene:bedroom-re-entry)
  #:go '(room:closet))



(define-cutscene (bedroom-description)
  (yield "The bedroom is in its usual untidy state."))



(define-cutscene (bedroom-re-entry)
  (yield "You stand in your bedroom."))



(define-room closet
  #:entry-cutscene '(cutscene:closet-entry)
  #:description '(cutscene:closet)
  #:re-entry-cutscene '(cutscene:closet-re-entry)
  #:go '(room:bedroom)
  #:look '(cutscene:suit cutscene:sludge-merch)
  #:commands (hash '("wear" "suit") 'cutscene:wear-suit
                   '("wear" "sludge merch") 'cutscene:wear-sludge-merch))



(define-cutscene (closet-entry)
  (yield "You step between the open doors of the closet."))



(define-cutscene (closet-re-entry)
  (yield "Having doubts, you check the closet again."))



(define-cutscene (closet)
  (yield
   (case (get-flag 'closet:taken)
     [(nothing) "The bedroom light has never quite been able to illuminate the depths of the closet. The floor and shelves, shrouded in darkness, are piled with dirty clothes that are waiting for a wash. Despite the mess, you can spot two fairly presentable outfits hanging on the rail. One of them is a formal blue suit you've been saving for a job interview. The other is a branded Sludge Co t-shirt you won in a raffle."]
     [(suit) "The Sludge Co t-shirt looks mockingly at you from the end of the rail. You try to push it out of your mind."]
     [(merch) "The suit remains on the rail dispassionately."])))



(define-cutscene (wear-suit)
  (set-flag 'closet:taken 'suit)
  (yield "You put on the suit."))



(define-cutscene (wear-sludge-merch)
  (set-flag 'closet:taken 'merch)
  (yield "You pull the hideous t-shirt over your torso, and find some complimenting shorts nearby. The colours almost don't clash."))
