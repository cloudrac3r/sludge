#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | Copyright 2024 Cadence Ember    |
;; | See `sludge-game/LICENSE.txt`   |
;; | for copying & reuse conditions. |
;; \_________________________________/


(require (for-syntax racket/base racket/match racket/set racket/string)
         racket/gui/easy)
(provide
 truthy
 @>
 hpanel-
 hpanel$
 hpanel*
 vpanel-
 vpanel$
 vpanel*)
(define (truthy v)
  (not (member v (list #f null 0 ""))))

(define-syntax (@> stx)
  (define (update-body-remove-@ body)
    (define collection-set (mutable-set))
    (define updated
      (let loop ([sexp body])
        (cond [(symbol? sexp)
               (let ([as-s (symbol->string sexp)])
                 (if (string-prefix? as-s "@")
                     (let ([without-@ (string->symbol (substring as-s 1))])
                       (set-add! collection-set (cons sexp without-@))
                       without-@)
                     sexp))]
              [(and (pair? sexp) (memq (car sexp) '(obs-peek :=)))
               (list* (car sexp) (cadr sexp) (loop (cddr sexp)))]
              [(pair? sexp)
               (cons (loop (car sexp)) (loop (cdr sexp)))]
              [#t sexp])))
    (values (set->list collection-set) updated))
  (define form (syntax->datum stx))
  (match form
    [(list '@> (? pair? body)) ; (@> (fn @obs))
     ;; identify the observables and replace with non-@ symbols
     (define-values (collection updated) (update-body-remove-@ body))
     ;; return obs-combine -> updated-form
     (datum->syntax stx `(obs-combine (λ (,@(map cdr collection))
                                        ,updated)
                                      ,@(map car collection)))]
    [(list '@> (? pair? body) (list 'else (? (compose1 not list?) otherwise))) ; (@> (fn @obs) (else ""))
     ;; same as above except if obs is falsy just return [otherwise] straight away
     (define-values (collection updated) (update-body-remove-@ body))
     (datum->syntax stx `(obs-combine (λ (,@(map cdr collection))
                                        ;; condition added here
                                        (if (and ,@(map cdr collection))
                                            ,updated
                                            ,otherwise))
                                      ,@(map car collection)))]
    [(list '@> (? string? str) args ...) ; (@> "Progress: ~a/~a ~a" (length @files) @total "files")
     ;; same as above except there are many args which are built into a format string
     (when (null? args)
       (raise-syntax-error #f "bad syntax: requires at least one observable after string" stx))
     (define-values (collection updated) (update-body-remove-@ args))
     (datum->syntax stx `(obs-combine (λ (,@(map cdr collection))
                                        ;; format string using all updated args
                                        (format ,str ,@updated))
                                      ,@(map car collection)))]))

(define-syntax-rule (hpanel- body ...)
  (hpanel #:stretch '(#t #f) body ...))
(define-syntax-rule (hpanel$ body ...)
  (hpanel #:stretch '(#f #t) body ...))
(define-syntax-rule (hpanel* body ...)
  (hpanel #:stretch '(#f #f) body ...))

(define-syntax-rule (vpanel- body ...)
  (vpanel #:stretch '(#t #f) body ...))
(define-syntax-rule (vpanel$ body ...)
  (vpanel #:stretch '(#f #t) body ...))
(define-syntax-rule (vpanel* body ...)
  (vpanel #:stretch '(#f #f) body ...))
