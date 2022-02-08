#lang info

(define deps
  '("base"
    "define2"
    "global"))

(define build-deps
  '("at-exp-lib" "rackunit-lib" "scribble-lib" "racket-doc"))

(define compile-omit-files
  '("wordle-solver-uai.rkt" "wordle-revealer.rkt"))

(define compile-omit-paths
  '("backups"))

#;
(define scribblings
  '(["README.scrbl" () (library) "wordle-solver"]))

(define license
  '(Apache-2.0 OR MIT))
