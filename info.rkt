#lang info
(define name "Sludge Ruckus")
(define version "0.1")
(define collection "ruckus")
(define deps '("base" "gui-lib" "math-lib" "opengl" "gui-easy-lib" "opengl" "struct-plus-plus"))

(define build-deps '("racket-doc"))

(define scribblings '(("doc/ruckus.scrbl" (multi-page))))
(define compile-omit-paths '("ex"))

(define racket-launcher-libraries
  '("viz/view3d.rkt"
    "viz/view2d.rkt"
    "viz/shaderdump.rkt"
    "out/outline.rkt"
    "out/surface.rkt"
    "lang/irdump.rkt"
    "lang/rktdump.rkt"
    "lang/astdump.rkt"))
(define racket-launcher-names
  '("ruckus-3d"
    "ruckus-2d"
    "ruckus-dump-glsl"
    "ruckus-export-outline"
    "ruckus-export-surface"
    "ruckus-dump-ir"
    "ruckus-dump-rkt"
    "ruckus-dump-ast"))
