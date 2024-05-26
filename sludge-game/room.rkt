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
         racket/set
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
  #:model 'bedroom
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



(define-room closet
  #:model 'closet
  #:entry-cutscene '(cutscene:closet-entry)
  #:description '(cutscene:closet)
  #:re-entry-cutscene '(cutscene:closet-re-entry)
  #:commands (hash '("go" "bedroom") 'room:bedroom
                   '("look" "suit") 'cutscene:closet-suit
                   '("look" "sludge merch") 'cutscene:closet-sludge-merch
                   '("wear" "suit") 'cutscene:closet-wear-suit
                   '("wear" "sludge merch") 'cutscene:closet-wear-sludge-merch))

(define-cutscene (closet-entry)
  (yield "You step between the open doors of the closet."))

(define-cutscene (closet-re-entry)
  (yield "Having doubts, you check the closet again."))

(define-cutscene (closet)
  (yield
   (case (get-flag 'closet:taken)
     [(nothing) "The bedroom light has never quite been able to illuminate its depths. The floor and shelves, shrouded in darkness, are piled with dirty clothes that are waiting for a wash. Despite the mess, you can spot two clean outfits hanging on the rail. One of them is a formal blue suit you've been saving for a job interview. The other is a branded Sludge Co t-shirt you won in a raffle."]
     [(suit) "The Sludge Co t-shirt looks mockingly at you from the end of the rail. You try to push it out of your mind."]
     [(merch) "The suit remains on the rail dispassionately."])))

(define-cutscene (closet-suit)
  (yield "It's a blue suit that you neatly ironed months ago when the world seemed to hold more hope and time. The clothes hanger also holds a dress shirt, and matching trousers are dangling over the rail."))

(define-cutscene (closet-sludge-merch)
  (yield "It's almost indescribably bad. It's a t-shirt that's mostly brown, but has a splatter pattern of red, green and yellow marks printed on it. It has the word \"SLUDGE\" diagonally across the front in a huge font that wouldn't feel out of place in a 2014 Minecraft video thumbnail. The inside label says, \"Show your Sludge Pride!\" Unlike the other t-shirts crumpled on the floor, this one is hanging on the clothes rail - not out of respect, but simply because you haven't ever worn it."))

(define-cutscene (closet-wear-suit)
  (set-flag 'closet:taken 'suit)
  (yield "You take the suit from its hanger and get dressed."))

(define-cutscene (closet-wear-sludge-merch)
  (set-flag 'closet:taken 'merch)
  (yield "You pull the hideous t-shirt over your torso, and find some complimenting shorts nearby. The colours almost don't clash."))



(define-room kitchen
  #:model 'kitchen
  #:description '(cutscene:kitchen-description)
  #:re-entry-cutscene '(cutscene:kitchen-re-entry)
  #:commands (hash '("go" "bedroom") 'room:bedroom
                   '("go" "living room") 'room:living-room
                   '("take" "water") 'cutscene:kitchen-take-water
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
  (hash-remove! (room-commands (hash-ref world 'room:kitchen)) '("take" "water")))

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
  #:model 'living-room
  #:description '(cutscene:living-room-description)
  #:re-entry-cutscene '(cutscene:living-room-re-entry)
  #:commands (hash '("go" "kitchen") 'room:kitchen
                   '("go" "front door") 'cutscene:front-door
                   '("look" "sofa") 'cutscene:living-room-sofa
                   '("look" "bookshelf") 'cutscene:living-room-bookshelf
                   '("look" "window") 'cutscene:living-room-window
                   '("look" "table") 'cutscene:living-room-table))

(define-cutscene (living-room-entry)
  (yield "You wander past the kitchen's stools and click the light switch."))

(define-cutscene (living-room-description)
  (yield "It's a compact, open-plan kitchen and living room. The walls are plain white. You spend most of your time in here, since thinking about the mess in your bedroom stresses you out."))

(define-cutscene (living-room-re-entry)
  (yield "You walk into the living room again. The table catches your attention."))

(define-cutscene (living-room-sofa)
  (yield "The large fabric sofa, covered in cushions of varying colours and sizes, is practically luxurious in this room. It's your favourite place to sit and relax."))

(define-cutscene (living-room-bookshelf)
  (yield "When you moved out here, you insisted on bringing the bookshelf with all the books you read as a teenager. There are so many books that some are piled on the shelves, obscuring the titles of the neatly arranged novels behind them. A large spider plant, which is somehow thriving despite your infrequent care, rests on the very top.")
  (when (not (eq? (get-flag 'water) 'watered))
    (hash-set! (room-commands (hash-ref world 'room:living-room)) '("water" "plant") 'cutscene:living-water-plant)))

(define-cutscene (living-water-plant)
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
         "Wowzers!\n\n0 new companies visited your profile this week. Keep up the great work!\n\nIn the last week:\n+0 views  /  +0 likes  /  +0 requests\n\nYours,\nCandidate+\n~We put the magic in hiring~")
    #("" "Waste Management" "Barrel Specialist: Application Rejected"
         "I hope this email finds you well.\n\nUnfortunately, after thinking long and hard, we will not be continuing with your application at this time. We feel that while your résumé showed immense experience in barrels, it didn't demonstrate enough expertise with casks or receptacles, and we decided to persue a more qualified candidate.\n\nYou are always encouraged to improve your skills and apply for a role in the future.\n\nKind regards,\n\nWynn Jackon\nWaste Management\n~Don't waste today - waste away!~")
    #("" "Dairy Life" "RE: Role as cowtaker"
         "Thanks for your interest in this role.\n\nTo protect the wellbeing of our livestock, we do require candidates to have prior experience in handling large animals before we can continue with the hiring process. You are welcome to re-apply if you undergo additional training in the future.\n\nBest,\n\nBlake\nSenior Cow Coordinator\nDairy Life")
    #("" "Recruit-o-matic" "Your Application Was Unsuccessful"
         "(You're too disappointed to read the rejection letter.)")
    #("" "Cashew Crew" "Your Application For Nut Crusher"
         "(You're too scared of it being a rejection letter to actually open it.)")))

(define-cutscene (laptop)
  (yield "You open your laptop. Some new emails have come in since you checked last night.")
  (define sem (make-semaphore))
  (yield (list (button "Check emails..." (λ () (semaphore-post sem)))
               sem))
  (define editor (new log-text%))
  (define r
    (render
     (window
      #:mixin (λ (%)
                (class %
                  (super-new)
                  (define/augment (on-close)
                    (semaphore-post sem))))
      #:title "Totally Legit Email Program"
      #:size '(660 700)
      (table
       #:style '(single column-headers)
       #:column-widths '((0 40) (1 160) (2 430))
       '("" "From" "Subject")
       emails
       #:entry->row (λ (entry) (vector-take entry 3))
       (λ (action table n)
         (when (and (eq? action 'select) n)
           (send editor erase)
           (send editor insert (vector-ref (vector-ref table n) 3))
           (send editor set-position 0 0)
           (when (= n 1)
             (set-flag 'has-interview #t)))))
      (editor-canvas editor #f))))
  (yield (list (button "All done." (λ () (semaphore-post sem)))
               sem))
  (renderer-destroy r)
  (when (get-flag 'has-interview)
    (yield "")
    (yield "Finally, after months of grinding, I landed a job interview!! I'd better get ready so I can make a good first impression.")))

(define-cutscene (front-door)
  (cond
    [(not (get-flag 'has-interview))
     (yield "It's even colder out there. You'd rather stay indoors.")]

    [(eq? (get-flag 'closet:taken) 'nothing)
     (yield "You can't leave until you've gotten dressed.")]

    [else (yield (list "go" 'room:outside))]))



(define-room outside
  #:entry-cutscene '(cutscene:outside-entry)
  #:description '(cutscene:outside-description)
  #:commands (hash '("look" "door") 'cutscene:outside-door
                   '("look" "sign") 'cutscene:outside-sign
                   '("look" "car park") 'cutscene:outside-car-park
                   '("knock" "") 'cutscene:outside-knock
                   '("go" "inside") 'cutscene:outside-try-go-inside))

(define (check-if-door-answered caller)
  (define waited (set-add (get-flag 'outside:waited) caller))
  (set-flag 'outside:waited waited)
  (when (and (>= (set-count waited) 4) (not (get-flag 'outside:door-unlocked)))
    (answer-door)))

(define-cutscene (outside-entry)
  (yield "At 2:30 PM, you find yourself outside the locked door of the Sludge Co building."))

(define-cutscene (outside-description)
  (yield "It looks industrial, which is to say, ugly. It doesn't need to look good - being out of the way in West Borcon, the only people who'd see it would be the employees arriving for the day. Or, in your case, soon-to-be employees?")
  (yield "Looks like you'll just have to wait for someone to let you in."))

(define-cutscene (outside-sign)
  (yield "You're in the right place. The sign outside the building says, in loopy cursive writing, \"Sludge Enterprises: Innovating Sludge Production Since 1961.\"")
  (check-if-door-answered 'sign))

(define-cutscene (outside-car-park)
  (yield "It says \"Employee Parking Only\", so just to be safe, you parked some distance down the road. The car park includes a device about the size of an ATM with large cables and pipes attached to it.")
  (hash-set! (room-commands (hash-ref world 'room:outside)) '("look" "device") 'cutscene:outside-device)
  (check-if-door-answered 'car-park))

(define-cutscene (outside-device)
  (yield "The device's instructions are hard to understand, but it seems to imply that you can park your car nearby and connect a tube to your car's exhaust in order to help the factory produce more sludge. It reminds you of try-it-yourself science museum exhibits, only dirtier. The device isn't currently being used.")
  (check-if-door-answered 'device))

(define-cutscene (outside-door)
  (yield "The glass door is electronically locked with a card reader.")
  (yield "Immediately past the glass door is a flight of stairs. The stairs go up so far you can't see what's at the top.")
  (check-if-door-answered 'door))

(define-cutscene (outside-answer-door) ;; Just used for the debug menu.
  (answer-door))

(define-cutscene (outside-knock)
  (if (get-flag 'outside:knocked)
      (yield "You knock again, harder, hurting your kuckles a bit. There is still no answer.")
      (yield "You knock firmly on the door. There is no answer from the top of the stairs."))
  (set-flag 'outside:knocked #t))

(define (answer-door)
  (yield "A man heads down the stairs and clicks the door open. \"Hi, I'm Chad. You must be here for your interview?\" He sticks out his hand.")
  (define sem (make-semaphore))
  (yield (list (hpanel (button "Shake his hand" (λ ()
                                                  (set-flag 'outside:handshake #t)
                                                  (semaphore-post sem)))
                       (button "Leave him hanging" (λ () (semaphore-post sem))))
               sem))
  (yield (cond [(get-flag 'outside:handshake) "You shake his hand. It's sticky."]
               [else "There is an awkward pause."]))
  (case (get-flag 'closet:taken)
    [(merch) (yield "You notice that he is smartly dressed. You try to read his expression, but you can't tell what he thinks of your Sludge Co t-shirt.")]
    [else (yield "You notice he is wearing a Sludge Co branded t-shirt.")])
  (yield "\"Well, come inside then, and let's show you how things work around here.\" He leads the way inside, up the stairs.")
  (set-flag 'outside:door-unlocked #t))

(define-cutscene (outside-try-go-inside)
  (cond
    [(get-flag 'outside:door-unlocked)
     (yield (list "go" 'room:office))]

    [else
     (yield "The door is locked. You'll have to wait.")]))



(define-room office
  #:entry-cutscene '(cutscene:office-entry)
  #:description '(cutscene:office-description)
  #:commands (hash '("look" "window") 'cutscene:office-window
                   '("look" "table") 'cutscene:office-table
                   '("look" "chairs") 'cutscene:office-chairs
                   '("look" "hole") 'cutscene:office-hole
                   '("go" "exit") 'cutscene:office-exit))

(define-cutscene (office-entry)
  (yield "Chad leads you off the stairs into a room."))

(define-cutscene (office-description)
  (yield "It is a small, uncomfortably warm office. The room's only window looks out onto a massive room that's lower in elevation. In the middle of this room is the interview setup: two chairs facing each other with a table in between."))

(define-cutscene (office-window)
  (yield "The window is perfectly clean. In the room below, workers are milling around a giant, swimming pool-sized vat of brown sludge. The sludge is full of large chunks. The workers are doing various tasks, some holding buckets, one stirring the sludge vat with a giant rake."))

(define-cutscene (office-table)
  (yield "On the table is just a stack of white cards, face down. Nervousness tickles your spine."))

(define-cutscene (office-hole)
  (cond
    [(get-flag 'office:lost-interview)
     (yield "You won't be able to stop thinking about this moment whenever you close your eyes.")]

    [else
     (yield "There's some kind of horizontal hole on the wall, like a post box. It's unclear what it's for.")]))

(define-cutscene (office-chairs)
  ;; EDITOR'S NOTE: I didn't realise this was basically Evrart's office from Disco Elysium until it was too late. No plagiarism intended.
  (yield "Facing the table are two chairs. The one closest to the door is a folding chair that looks uncomfortably small. The other looks almost like a mahogany throne and a high, cushioned back. At once you realise: this is not the office of a corporate leader, but of an emperor.")
  (hash-set! (room-commands (hash-ref world 'room:office)) '("sit" "folding chair") 'cutscene:office-folding-chair)
  (hash-set! (room-commands (hash-ref world 'room:office)) '("sit" "throne") 'cutscene:office-throne))

(define-cutscene (office-throne)
  (yield "Chad chuckles. \"Heh... don't get too ahead of yourself.\""))

(struct card (name one two design) #:transparent)

(define cards
  (list (card "Working Together" "Team Player" "Lone Ranger" #f)
        (card "Output" "Quality" "Quantity" #f)
        (card "Agitation" "Patient" "Go-Getter" #f)
        (card "Morals" "Good Person" "Bad Person" #f)
        (card "Faith" "Love the Sinner" "Hate the Sin" #f)
        (card "Geometry" "Rhombic Dodecahedron" "Gömböc" 'rhombic-gomboc)
        (card "Your Alibi For 31 August 1997" "Concrete" "Flimsy" #f)
        (card "Traffic Light" "No" "Yes" #f)))

(define questions
  (list #(equal-opportunity
          "Are you an equal opportunity employer?"
          "Um... of course! Besides being legally required to say yes, here at Sludge Co we really don't care about your identity. Are your pronouns sludge/gloop? No problem for us. All we need is for you to bring a sludgy attitude to work each day. And we need the personality test to be compatible, but that's just a formality."
          ())
        #(safety
          "What kind of safety procedures do you use?"
          "Well, we've put up all these warning signs anywhere you might come in to contact with sludge. That means we're legally okay in the event of an accident. Besides, it's quite safe really. The worst thing that could happen to you here is falling into the sludge. And if that happened... while your family would miss you, rest assured the cash flow from the top quality sludge produced by your body would let you have a *really* nice funeral."
          ())
        #(purpose
          "So you... produce the sludge?"
          "Yes, the sludge is our creation! We're world-famous for being the number one producer of all things gloopy, filthy, slimy and sticky. But you knew that already, right? Ha ha ha..."
          (makeup buyers why-buy))
        #(makeup "What is the sludge made of?"
                 "Oh, it's made of all sorts of things! You name it, it's probably in there." ())
        #(buyers "Who's the main buyer of all this sludge?"
                 "We have many business partners around the world. They all want a piece of our sludge! There's no main buyer per se, but the way it usually goes is, if they have money, they buy sludge. And if they have more money they buy more sludge." ())
        #(why-buy "Why do people want the sludge? What is it for?"
                  "I mean... look at it!\" He gestures out the window towards the sludge pool in the other room. \"It's a really innovative product - limitless sludge on demand. Who wouldn't want it? And as long as the orders keep coming in, we'll keep filling them." ())))
(define/obs @enabled-questions (seteq 'equal-opportunity 'safety 'purpose))
(define @visible-questions
  (@> (for/vector ([q (in-list questions)]
                   #:when (set-member? @enabled-questions (vector-ref q 0)))
        q)))

(define-cutscene (office-folding-chair)
  (yield "You sit in the folding chair and Chad sits in his throne.")
  (yield "\"Alright. So first of all, I'll get you to answer this personality test. And then after that, you'll get to ask me some questions about what we do here. Let's get right into it. Would you mind turning over the first card?\"")
  (yield "\"For each question, please use the slider to determine how much you identify with the statement on the card.\"")
  (define sem (make-semaphore))
  (yield (list (button "Look at card" (λ () (semaphore-post sem)))
               sem))
  (for ([card cards]
        [i (in-naturals 1)])
    (define/obs @p 50)
    (yield (list "design" (card-design card)))
    (yield (list (vpanel
                  (hpanel- (text "") (spacer)
                           (text (format "** Theme: ~a **" (card-name card))) (spacer)
                           (text (format "~a/~a" i (length cards))))
                  (hpanel- (text (@> "~a ~a%" (card-one card) (- 100 @p))) (spacer)
                           (text "- - - - -") (spacer)
                           (text (@> "~a% ~a" @p (card-two card))))
                  (slider #:style '(horizontal plain)
                          @p
                          (λ:= @p))
                  (button "OK" (λ () (semaphore-post sem))))
                 sem)))
  (yield (list "prompt" "You finish filling out the cards."))
  (yield "\"Great, thanks for filling that out for us.\" He takes the cards and slides them into the hole on the wall. \"Our AI will analyse that shortly. Now it's your turn. Did you have any questions about the company?")
  (your-turn-for-questions))

(define-cutscene (office-your-turn-for-questions) ;; Just used for the debug menu.
  (your-turn-for-questions))

(define (your-turn-for-questions)
  (define sem (make-semaphore))
  (yield "\"Oh, and before you ask - no, human bodies do not contain microsludge particles. That's a lie made up by doctors, environmentalists, and concerned denizens of the world - you can't trust any of them.\"")  
  (let loop ()
    (define continue #f)
    (yield (list (hpanel-
                  (button "Ask..." (λ () (set! continue #t) (semaphore-post sem)))
                  (button "All done." (λ () (semaphore-post sem))))
                 sem))
    (when continue
      (define/obs @selection 0)
      (define r
        (render
         (window
          #:title "What do you want to ask?"
          #:size '(400 300)
          #:mixin (λ (%)
                    (class %
                      (super-new)
                      (define/augment (on-close)
                        (set! continue #f)
                        (semaphore-post sem))))
          (table
           '("Select question")
           @visible-questions
           #:entry->row (λ (q) (vector (vector-ref q 1)))
           (λ (action table n)
             (when (and (eq? action 'select) n)
               (:= @selection n)))
           #:style '(single column-headers)
           #:selection @selection)
          (button "Ask" #:enabled? (@> (not (not @selection))) (λ () (semaphore-post sem))))))
      (yield (list (spacer)
                   sem))
      (renderer-destroy r)
      (when (and continue (obs-peek @selection))
        (define chosen (vector-ref (obs-peek @visible-questions) (obs-peek @selection)))
        (yield (list "prompt" (vector-ref chosen 1)))
        (yield (format "\"~a\"" (vector-ref chosen 2)))
        (@enabled-questions . <~ . (λ (e) (set-union e (apply seteq (vector-ref chosen 3)))))
        (loop)))
    (void))

  (set-flag 'office:lost-interview #t)
  (hash-remove! (room-commands (hash-ref world 'room:office)) '("look" "chairs"))
  (hash-remove! (room-commands (hash-ref world 'room:office)) '("sit" "folding chair"))
  (hash-remove! (room-commands (hash-ref world 'room:office)) '("sit" "throne"))
  (yield (list "prompt" "You finish asking your questions."))
  (yield "There is a whirring nearby, followed by a *ding!*. Chad gets up and walks to the slot in the wall, and retrieves a paper printout from it. He unfolds it, and narrows his eyes as he reads. \"Alright, so the results of the personality test are back...\"")
  (gui:yield)
  (yield "You hold your breath. \"And it says you're kind of a loser. Huh, never seen that one before. Well, unfortunately we won't be hiring you today.\"")
  (gui:yield))

(define-cutscene (office-exit)
  (cond
    [(get-flag 'office:lost-interview)
     (yield "You leave without saying a word. The events of the day keep replaying in your head as you drive home.")
     (yield "After you get home, you check your laptop. There is a new email from Sludge Co.")
     (define sem (make-semaphore))
     (yield (list (button "Read email" (λ () (semaphore-post sem)))
                  sem))
     (define editor (new log-text%))
     (define r
       (render
        (window
         #:title "Thank you!"
         #:size '(510 400)
         (editor-canvas editor #f))))
     (send editor erase)
     (send editor insert "We're sorry to hear about your recent interview.\n\nWhile you weren't successful this time, we're always keen on adding new members to our team. To assist in future hiring, a report of your interview performance was automatically generated and sent to our subsidiaries:\n\nSLUDGE ENGINE\n* Cadence Ember\n\nSLUDGE WRITING\n* Cadence Ember\n* James Havoc\n\nRUCKUS 3D VISUALISER\n* Cliff L. Biffle\n\nGRAPHICAL SUPPORT\n* Bogdan Popa\n\nRACKET PROGRAMMING ENVIRONMENT\n* https://racket-lang.org/team.html\n\nThank you for playing this SLUDGE FICTION interactive experience!")
     (send editor set-position 0 0)
     (yield (list (text "THE END")
                  sem))
     (renderer-destroy r)]

    [else
     (yield "There is no escape.")]))



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
