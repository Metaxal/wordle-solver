#lang scribble/manual
@(require racket/list
          racket/port
          (for-syntax racket/base)
          (for-label racket/contract/base
                     racket/base
                     wordle-solver
                     global)
          wordle-solver)

@title{Wordle solver: A simple greedy solver for Wordle}

See the @hyperlink["https://github.com/Metaxal/wordle-solver/blob/master/README.md"]{README} for a
quick start and usage examples.

@defmodule[wordle-solver]

@bold{Note:} The interface might change in the future. The documentation below describes only a
subset of the exported forms.

@defproc[(guess? [x any/c]) boolean/c]{A 5-letter word with only alphabetic characters.}
@defproc[(clue/int? [x any/c]) boolean/c]{A number between 0 and 243 (=3⁵).}
@defproc[(history? [x any/c]) boolean/c]{
Returns @racket[#true] if @racket[x] matches @racket[(listof (list/c guess? clue/int?))]}

@defproc[(play [target guess?]
               [goals (listof guess?)]
               [allowed (listof guess?)]
               [#:history-hash history-hash hash? (make-hash)])
         history?]{
The main function to play a game, either with user interaction or automatically.
Its behaviour depends on several globals described below.
}


@defthing*[([*first-word* global?]
            [*auto?* global?]
            [*silent?* global?]
            [*squares?* global?]
            [*consistent-only?* global?]
            [*share-social?* global?]
            )]{
The globals are also used as command line arguments.
@verbatim|{racket -l- wordle-solver --help}| outputs:
@(verbatim
  (with-output-to-string
   (λ ()
     (let/ec return
       (parameterize ([current-command-line-arguments #("--help")]
                      [exit-handler (λ (v) (return v))])
         (main #:play play))))))}
