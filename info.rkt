#lang info

(define collection "wordle-solver")

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

(define scribblings
  '(["scribblings/wordle-solver.scrbl" ()]))

(define version "0.0")

(define pkg-desc "Wordle solver: A simple greedy solver for Wordle")

(define pkg-authors '(lorseau))

(define license
  '(Apache-2.0 OR MIT))
