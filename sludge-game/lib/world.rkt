#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require racket/generator
         struct-plus-plus)
(require (for-syntax racket/base
                     syntax/parse))
(provide (all-defined-out))

(define world (make-hasheq))

(struct++ room ([id symbol?]
                [name string?]
                [(entry-cutscene null) list?]
                [(description null) list?]
                [(re-entry-cutscene null) list?]
                [(go null) list?]
                [(look null) list?]
                [(commands (hash)) hash?])
          #:transparent)
(struct++ cutscene (id def) #:transparent)

(define-syntax (define-room stx)
  (syntax-parse stx
    [(_ name:id body ...+)
     (define name-string (symbol->string (syntax->datum #'name)))
     (define namespace-name (string->symbol (format "room:~a" name-string)))
     #`(begin
         (define #,namespace-name
           (room++
            #:id '#,namespace-name
            #:name #,name-string
            body ...))
         (hash-set! world '#,namespace-name #,namespace-name))]))

(define-syntax (define-cutscene stx)
  (syntax-parse stx
    [(_ (name:id) body ...+)
     (define namespace-name (string->symbol (format "cutscene:~a" (symbol->string (syntax->datum #'name)))))
     #`(begin
         (define #,namespace-name
           (cutscene
            '#,namespace-name
            (λ () (generator () body ... (void)))))
         (hash-set! world '#,namespace-name #,namespace-name))]))
