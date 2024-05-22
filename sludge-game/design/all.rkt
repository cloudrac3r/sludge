#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (design stx)
  (syntax-parse stx
    ((_ name:id ...)
     #'(begin
         (define all (make-immutable-hasheq (list (cons 'name name) ...)))
         (provide all)
         (design* name ...)))))

(define-syntax (design* stx)
  (syntax-parse stx
    [(_ name:id rest ...)
     (define path (string-append (symbol->string (syntax->datum #'name)) ".rkt"))
     #`(begin
         (require (rename-in (submod #,path ruckus-metadata) [top-level-thunk name]))
         (provide name)
         (design* rest ...))]
    [_
     #'(void)]))

(design table cube rhombic-gomboc)
