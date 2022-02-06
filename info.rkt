#lang info

(define deps
  '("base"))

(define build-deps
  '("at-exp-lib" "rackunit-lib" "scribble-lib" "racket-doc"))

(define compile-omit-files
  '("wordle-solver-uai.rkt" "wordle-revealer.rkt"))

#;
(define scribblings
  '(["README.scrbl" () (library) "wordle-solver"]))
