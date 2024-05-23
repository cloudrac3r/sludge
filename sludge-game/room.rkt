#lang racket/base

;; |‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾:.
;; | Copyright 2024 Cadence Ember        : '.
;; | This file is licensed under the     :...:.
;; | Creative Commons BY-NC license.          '|
;; | That means you can distribute and adapt   |
;; | this file for non-commercial purposes     |
;; | only, in any medium, as long as you give  |
;; | credit. See here for full license terms:  |
;; http://creativecommons.org/licenses/by-nc/4.0
;; |___________________________________________|


(require racket/generator
         racket/gui/easy
         racket/gui/easy/operator
         "lib/flags.rkt"
         "lib/world.rkt")
(provide (all-defined-out))

(define/obs @current-room-id 'room:bedroom)



(define-room bedroom
  #:entry-cutscene '(cutscene:bedroom-entry)
  #:description '(cutscene:bedroom-description)
  #:re-entry-cutscene '(cutscene:bedroom-re-entry)
  #:commands (hash '("go" "closet") 'room:closet
                   '("go" "front door") 'cutscene:front-door))



(define-cutscene (front-door)
  (case (get-flag 'closet:taken)
    [(nothing) (yield "You can't leave until you've gotten dressed.")]
    [else (yield (list "go" 'room:work))]))


(define-room work
  #:description '(cutscene:work-description))



(define-cutscene (work-description)
  (yield "Welcome to work."))



(define-cutscene (bedroom-entry)
  (yield "6:00 AM. Your alarm is beeping.")
  (yield "You begrudgingly turn it off and swing your legs out of bed, feet landing on the cold carpeted floor."))



(define-cutscene (bedroom-description)
  (yield "Your bedroom is in its usual untidy state."))



(define-cutscene (bedroom-re-entry)
  (yield "You stand in your bedroom."))



(define-room closet
  #:entry-cutscene '(cutscene:closet-entry)
  #:description '(cutscene:closet)
  #:re-entry-cutscene '(cutscene:closet-re-entry)
  #:commands (hash '("go" "bedroom") 'room:bedroom
                   '("look" "suit") 'cutscene:suit
                   '("look" "sludge merch") 'cutscene:sludge-merch
                   '("wear" "suit") 'cutscene:wear-suit
                   '("wear" "sludge merch") 'cutscene:wear-sludge-merch))



(define-cutscene (closet-entry)
  (yield "You step between the open doors of the closet."))



(define-cutscene (closet-re-entry)
  (yield "Having doubts, you check the closet again."))



(define-cutscene (closet)
  (yield
   (case (get-flag 'closet:taken)
     [(nothing) "The bedroom light has never quite been able to illuminate its depths. The floor and shelves, shrouded in darkness, are piled with dirty clothes that are waiting for a wash. Despite the mess, you can spot two fairly presentable outfits hanging on the rail. One of them is a formal blue suit you've been saving for a job interview. The other is a branded Sludge Co t-shirt you won in a raffle."]
     [(suit) "The Sludge Co t-shirt looks mockingly at you from the end of the rail. You try to push it out of your mind."]
     [(merch) "The suit remains on the rail dispassionately."])))



(define-cutscene (suit)
  (yield "It's a blue suit that you neatly ironed months ago when the world seemed to hold more hope and time. The neighbouring clothes hanger holds a dress shirt, and matching trousers are dangling over the rail."))



(define-cutscene (sludge-merch)
  (yield "It is indescribable."))



(define-cutscene (wear-suit)
  (set-flag 'closet:taken 'suit)
  (yield "You put on the suit."))



(define-cutscene (wear-sludge-merch)
  (set-flag 'closet:taken 'merch)
  (yield "You pull the hideous t-shirt over your torso, and find some complimenting shorts nearby. The colours almost don't clash."))
