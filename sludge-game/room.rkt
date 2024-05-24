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


(require racket/class
         racket/generator
         racket/vector
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         "lib/at-map.rkt"
         "lib/flags.rkt"
         "lib/log.rkt"
         "lib/world.rkt")
(provide (all-defined-out))

(define/obs @current-room-id 'room:bedroom)



(define-room bedroom
  #:entry-cutscene '(cutscene:bedroom-entry)
  #:description '(cutscene:bedroom-description)
  #:re-entry-cutscene '(cutscene:bedroom-re-entry)
  #:commands (hash '("go" "closet") 'room:closet
                   '("go" "kitchen") 'room:kitchen
                   '("go" "bed") 'cutscene:bed))

(define-cutscene (bedroom-entry)
  (yield "6:00 AM. Your alarm is beeping.")
  (yield "You begrudgingly turn it off and swing your legs out of bed, feet landing on the cold carpeted floor."))

(define-cutscene (bedroom-description)
  (yield "Your bedroom is in its usual untidy state."))

(define-cutscene (bedroom-re-entry)
  (yield "You stand in your bedroom."))

(define-cutscene (bed)
  (define sem (make-semaphore))
  (define sleepy #f)
  (yield "Are you SURE you want to go back to bed?")
  (yield (list (hpanel (button "Ahh... must... stay... up..." (λ () (semaphore-post sem)))
                       (button "So sleepy..." (λ () (set! sleepy #t) (semaphore-post sem))))
               sem))
  (if sleepy
      (begin
        (yield ">\"So sleepy...\"")
        (yield (list (hpanel (button "..." (λ () (semaphore-post sem))))
                     sem))
        (yield ">*snore*")
        (yield (list (hpanel (text "... snore ..."))
                     sem)))
      (begin
        (yield ">\"Ahh... must... stay... up...\"")
        (yield "You force yourself to open your eyes wider. Your efforts are rewarded with being able to see the trash around the edges of the room."))))



(define-cutscene (front-door)
  (cond
    [(not (get-flag 'has-interview))
     (yield "It's even colder out there. You'd rather stay indoors.")]

    [(eq? (get-flag 'closet:taken) 'nothing)
     (yield "You can't leave until you've gotten dressed.")]

    [else (yield (list "go" 'room:work))]))


(define-room work
  #:description '(cutscene:work-description))

(define-cutscene (work-description)
  (yield "Welcome to work."))



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
  (yield "You take the suit off its hanger and get dressed."))

(define-cutscene (wear-sludge-merch)
  (set-flag 'closet:taken 'merch)
  (yield "You pull the hideous t-shirt over your torso, and find some complimenting shorts nearby. The colours almost don't clash."))



(define-room kitchen
  #:description '(cutscene:kitchen-description)
  #:re-entry-cutscene '(cutscene:kitchen-re-entry)
  #:commands (hash '("go" "bedroom") 'room:bedroom
                   '("go" "living room") 'room:living-room
                   '("take" "glass of water") 'cutscene:kitchen-take-water
                   '("look" "floor") 'cutscene:kitchen-floor
                   '("look" "sink") 'cutscene:kitchen-sink
                   '("look" "cupboard") 'cutscene:kitchen-cupboard
                   '("look" "fridge") 'cutscene:kitchen-fridge))

(define-cutscene (kitchen-entry)
  (yield "You yawn and head through the hallway into the kitchen."))

(define-cutscene (kitchen-description)
  (yield "Thankfully you left the kitchen tidier than your bedroom. Surfaces glitter all around. There's a slight scent of ginger in the air.")
  (when (eq? (get-flag 'water) 'kitchen)
    (yield "A glass of water sits on the counter.")))

(define-cutscene (kitchen-take-water)
  (yield "You take the glass of water.")
  (set-flag 'water 'taken)
  (hash-remove! (room-commands (hash-ref world 'room:kitchen)) '("take" "glass of water")))

(define-cutscene (kitchen-re-entry)
  (yield "You head back into the kitchen."))

(define-cutscene (kitchen-floor)
  (yield "You swept the kitchen floor earlier this week, so it barely has any crumbs on it."))

(define-cutscene (kitchen-sink)
  (yield "Last night's cleaned dishes sparkle in the drying rack. There's a sponge and a brush resting in the sink."))

(define-cutscene (kitchen-cupboard)
  (yield "The cupboard is stocked with staple foods. You can see cans of beans, lentils, spaghetti, flour, eggs, soy sauce, barbecue sauce, siracha sauce, and countless other foods. You don't feel like making anything right now."))

(define-cutscene (kitchen-fridge)
  (yield "All the shelves have been removed from the fridge to make space for an 8 litre bottle of milk. There is nothing else in the fridge."))



(define-room living-room
  #:description '(cutscene:living-room-description)
  #:re-entry-cutscene '(cutscene:living-room-re-entry)
  #:commands (hash '("go" "kitchen") 'room:kitchen
                   '("go" "bedroom") 'room:bedroom
                   '("look" "sofa") 'cutscene:living-room-sofa
                   '("look" "bookshelf") 'cutscene:living-room-bookshelf
                   '("look" "window") 'cutscene:living-room-window
                   '("look" "table") 'cutscene:living-room-table))

(define-cutscene (living-room-entry)
  (yield "You wander past the kitchen's stools and click the light switch."))

(define-cutscene (living-room-description)
  (yield "It's a compact, open-plan kitchen and living room. The walls are plain white. You spend most of your time in here, since thinking about the mess in your bedroom stresses you out."))

(define-cutscene (living-room-re-entry)
  (yield "You wander back into the living room, feeling a sense of calm wash over you."))

(define-cutscene (living-room-sofa)
  (yield "The large fabric sofa, covered in cushions of varying colours and sizes, is practically luxurious in this room. It's your favourite place to sit and relax."))

(define-cutscene (living-room-bookshelf)
  (yield "When you moved out here, you insisted on bringing the bookshelf with all the books you read as a teenager. There are so many books that some are piled on the shelves, obscuring the titles of the neatly arranged novels behind them. A large spider plant, which is somehow thriving despite your infrequent care, rests on the very top.")
  (when (not (eq? (get-flag 'water) 'watered))
    (hash-set! (room-commands (hash-ref world 'room:living-room)) '("water" "plant") 'cutscene:water-plant)))

(define-cutscene (water-plant)
  (cond
    [(eq? (get-flag 'water) 'taken)
     (yield "You gently pour the water onto the plant. Naturally, it doesn't respond, but you like to think it appreciates your care. A small amount of water slowly dribbles out the bottom of the pot.")
     (set-flag 'water 'watered)
     (hash-remove! (room-commands (hash-ref world 'room:living-room)) '("water" "plant"))]
    [else
     (yield "You'd need some water first.")]))

(define-cutscene (living-room-window)
  (yield "The curtains are pulled closed, but even if they were open, it's too dark to see anything this early on a winter day."))

(define-cutscene (living-room-table)
  (yield "On one corner of the table is your laptop. The rest of the table has newspaper and magazine cutouts strewn all over it. The cutouts are a work in progress - it was going to be a scrapbooking project that an influencer convinced you to start. You've grown too attached to the pictures to throw them away now.")
  (hash-set! (room-commands (hash-ref world 'room:living-room)) '("look" "laptop") 'cutscene:laptop))

(define emails
  #(#("📫" "Pharmabrew" "You Must Try THESE 5 Medications Before You Die"
           "You won't BELIEVE how effective these simple medications are at making you healthier, happier, and longer living! You simply MUST try them!\n\n1. Estradiol\n2. Estradiol\n3. Estradiol\n4. Estradiol\n5. Cyproterone Acetate\n\nClick here to buy now!!")
    #("📫" "Sludge Co." "Your Interview Schedule"
           "Congratulations on making it this far! We are pleased to offer you an interview for your application for the Sludge Handler role.\n\nYour interview is scheduled for 2:30 PM today at Sludge Co Headquarters, 221 Lemur Street, West Borcon. Please ring the buzzer and Chad will be down to see you.\n\nThank you for applying for this position at Sludge Co.\n\nKind regards,\n\nNusan White\nSlude-Human Relations\nSludge Co.")
    #("📫" "RCO News" "Today's Headlines"
           "BREAKING NEWS\n\n** Why leaving your house might be less healthy than you thought **\n\n** \"No Escape\": 70% of human organs contain microsludge, studies say **\n\n** Car crash leaves 3 wounded, stable condition **\n\nSee our website for the full stories.")
    #("" "Borcon Council" "Your Application Status"
         "Dear [Candidate's First Name],\n\nAfter careful consideration, we have decided not to move forward with your application at this time. We received a significant number of applications from qualified candidates like yourself, making this selection process extremely difficult.\n\nDue to the high volume of applications, we are not able to provide individual feedback to candidates. However, we do hope you'll stay connected with us and keep an eye on our future career opportunities. If a suitable position opens up again, we would love to hear from you and consider your application.\n\nWe wish you all the best in your job search.\n\nBorcon\nBuilding A Better Future")
    #("" "Candidate+" "0 New Companies Saw Your Profile This Week"
         "Dear [Insert First Name Here],\n\n0 new companies visited your profile this week. Keep up the great work!\n\nIn the last week:\n+0 views  /  +0 likes  /  +0 requests\n\nYours,\nCandidate+\n~We put the magic in hiring~")
    #("" "Waste Management" "Barrel Specialist: Application Rejected"
         "I hope this email finds you well.\n\nUnfortunately, after thinking long and hard, we will not be continuing with your application at this time. We feel that while your résumé showed immense experience in barrels, it didn't demonstrate enough expertise with casks or receptacles, and we decided to persue a more qualified candidate.\n\nYou are always encouraged to improve your skills and apply for a role in the future.\n\nKind regards,\n\nWynn Jackon\nWaste Management\n~Don't waste today - waste away!~")
    #("" "Dairy Life" "RE: Role as cowtaker"
         "Thanks for your interest in this role.\n\nSadly, we do require candidates to have prior experience in handling large animals before we can continue with the hiring process. You are welcome to re-apply if you undergo additional training in the future.\n\nBest,\n\nBlake\nSenior Cow Coordinator\nDairy Life")
    #("" "Recruit-o-matic" "Your Application Was Unsuccessful"
         "(You're too disappointed to read the rejection letter.)")
    #("" "Crew Cashew" "Your Application For Nut Crusher"
         "(You're too disappointed to read the rejection letter.)")))

(define-cutscene (laptop)
  (yield "You open your laptop. Some new emails have come in since you checked last night.")
  (define sem (make-semaphore))
  (yield (list (button "Check emails..." (λ () (semaphore-post sem)))
               sem))
  (define editor (new log-text%))
  (define r
    (render
     (window
      #:title "Totally Legit Email Program"
      #:size '(660 700)
      (table
       #:style '(single column-headers)
       #:column-widths '((0 40) (1 160) (2 430))
       '("" "From" "Subject")
       emails
       #:entry->row (λ (entry) (vector-take entry 3))
       (λ (action table n)
         (when (eq? action 'select)
           (send editor erase)
           (send editor insert (vector-ref (vector-ref table n) 3))
           (send editor set-position 0 0))))
      (editor-canvas editor #f))))
  (yield (list (button "All done." (λ () (semaphore-post sem)))
               sem))
  (renderer-destroy r))



(module+ test
  (define (check room-id k)
    (when (not (hash-has-key? world k))
      (printf "~a references ~a which does not exist~n" room-id k)))
  (for ([(world-id v) (in-hash world)])
    (cond
      [(room? v)
       ;; check that declared cutscenes exist
       (for ([attribute (append (room-entry-cutscene v)
                                (room-description v)
                                (room-re-entry-cutscene v))])
         (check world-id attribute))
       ;; check that declared commands exist
       (for ([(k v) (in-hash (room-commands v))])
         (check world-id v))]

      [else ; (cutscene? v)
       ;; try the generator and see what happens
       (define gen ((cutscene-def v)))
       (let loop ([i 0])
         (cond
           [(i . > . 100)
            (printf "~a: looped more than 100 times~n" world-id)]
           [(not (void? (gen)))
            (loop (add1 i))]))])))
