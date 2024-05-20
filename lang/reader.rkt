#lang s-exp syntax/module-reader

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾
;; | Modified by Cadence Ember
;; | Edited module to a relative path to support built-in models
;; \__________________________


; This file gets picked up by Racket when it finds '#lang ruckus'.  We use
; syntax/module-reader to redirect attention at mlang.rkt.

"./lang/mlang.rkt"
