#lang racket/gui

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;; | Modified by Cadence Ember
;; | Replaced design-path with design-thunk to support included pre-compiled models
;; | Removed debug logs and F5 keybind for Sludge
;; \__________________________


; Basic GL viewer with design loading and recompilation support.

(require opengl)
(require "../lang/loader.rkt")
(require "../lang/evaluator.rkt")
(require "../core/compiler/canon.rkt")
(require "../core/compiler/enumerate.rkt")

(provide gl-viewer%)

; The gl-viewer% widget provides our model-viewing facility with some niceties.
(define gl-viewer%
  (class canvas%
    (super-new)
    (inherit with-gl-context swap-gl-buffers)

    ; The path to the design file.  TODO: remove the default once this is done.
    (init-field design-thunk)

    ; Have we tried to load the design at least once?  This is used to trigger
    ; an immediate reload on first startup, without entering a reload loop
    ; should we fail.
    (field [tried-to-load-once #f])

    ; Has setup been called?  If false, the next on-paint event will call setup
    ; (with a valid GL context) before drawing.  Reloading the design will
    ; reset this to #f.
    (field [setup-called #f])

    ; Most recently compiled node (AST) representation of the design, used to
    ; forward the result of a reload from a key event to setup/on-paint.
    (field [design-node #f])

    ; Flag used to coalesce queued low-priority refresh events.
    (field [refresh-queued #f])

    ; Triggers a low-priority refresh event.  Display refresh events in Racket
    ; are normally higher priority than input events, which ensures that we'll
    ; get one refresh per e.g. mouse movement.  This is really bad if refresh is
    ; expensive.  This low-priority refresh mechanism lets us easily coalesce
    ; many input events into a single refresh.
    (define/public (low-priority-refresh)
      (unless refresh-queued
        (queue-callback (lambda ()
                          (send this refresh)
                          (set! refresh-queued #f))
                        #f)  ; <-- makes it low priority
        (set! refresh-queued #t)))

    ; Draw the scene.
    (define/overment (on-paint)
      (with-gl-context               
        (thunk
          (unless tried-to-load-once
            (set! tried-to-load-once #t)
            (reload))
          ; Lazily call setup if required.
          (unless setup-called
            (setup)
            (set! setup-called #t))
          (glPushMatrix)
          (inner (void) on-paint)
          (glPopMatrix)
          (swap-gl-buffers))))

    (define/overment (on-char event)
      (case (send event get-key-code)
        #;[(f5) (reload) (low-priority-refresh)]
        [else (inner (void) on-char event)]))

    (define/public (reload)
      ;; (printf "Recompiling design at ~a~n" design-path)
      (let ([gen design-thunk])
        (unless (procedure? gen)
          ; This handles silly cases like '(define design 3)'.
          (error 'viewer-reload "Design binds `design' to ~v, which is not a procedure" gen))

        (let-values ([(count node) (enumerate-nodes 0
                                     (first (canonicalize
                                              (call-with-edsl-root gen))))])
          #;(printf "Design contains ~a nodes.~n" count)
          (set! setup-called #f)
          (set! design-node node)
          (low-priority-refresh))))

    (abstract setup)
    ))
