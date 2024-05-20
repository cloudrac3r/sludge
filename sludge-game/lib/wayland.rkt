#lang racket/base

;; /‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾\
;; | To the extent possible under law, Cadence Ember has waived      |
;; | all copyright and related or neighboring rights to this file,   |
;; | as it is written in the following disclaimers:                  |
;; \__.- http://unlicense.org                                        |
;; | o|- http://creativecommons.org/publicdomain/zero/1.0/          o|
;; \o_|____________________________________________________________o_/


(require racket/system)

(when (and (eq? (system-type 'os) 'unix) (getenv "WAYLAND_DISPLAY") (not (getenv "GDK_BACKEND")))
  (putenv "GDK_BACKEND" "x11")
  (define-values (x r)
    (values
     (find-executable-path (find-system-path 'exec-file))
     (find-system-path 'run-file)))
  (printf "Re-execing `env GDK_BACKEND=x11 ~a ~a'...~n" (path->string x) (path->string r))
  (exit (system*/exit-code x r)))
