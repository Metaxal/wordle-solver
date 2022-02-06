#lang racket/base
(require racket/dict
         racket/list
         racket/pretty
         racket/string
         racket/format
         racket/port
         racket/random
         (for-syntax racket/base syntax/parse)
         define2
         global)

;;; Author: Laurent Orseau https://github.com/Metaxal
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.

;;; First, try:
;;;   racket wordle-solver.rkt --help
;;;
;;; To play the game manually:
;;;   racket wordle-solver.rkt --target random --no-auto --share-social
;;;
;;; It's also pretty fun to watch the memoization do its work and accelerate the search
;;; as more and more words are solved (very first word is slow to solve):
;;;   racket wordle-solver.rkt --target all --silent
;;; or for a longer watch:
;;;   racket wordle-solver.rkt --target all --silent --goals allowed.txt

(provide (all-defined-out))

(module+ test (require rackunit))

(define-global:boolean *silent?* #false
  "Hide most output?")

(define-global:boolean *consistent-only?* #false
  "Allow only guesses that are consistent with all seen clues? (set it to 'true' for 'hard mode'.)")

(define-global:boolean *auto?* #true
  "In auto mode, the word is guessed automatically; otherwise the user is queried.")

(define-global:boolean *squares?* #true
  "Print squares instead of letters for clues?")

(define-global:boolean *share-social?* #f
  "Display summary to share on social media?")

;;; clue: (or/c 'B 'O 'G)
;;; B(lack): not in word, O(range): in word but wrong place, G(reen): in word but right place
;;;
;;; > If the solution was “arrow” and you typed “mamas”, you’d get a yellow box for each “a”

;;; Greedy goal: minimize the size of the remaining set in the worst case

(define word-len 5)
(define n-possible-clues (expt 3 word-len))

(define (clue-win? c) (= 0 c))  ; 0 is "GGGGG" in integer encoding

(define (history-win? history)
  (clue-win? (second (first history))))

(define (guess? str)
  (and (= word-len (string-length str))
       (andmap char-alphabetic? (string->list str))))

(define (clue/string? str)
  (and (= word-len (string-length str))
       (for/and ([c str]) (member c '(#\B #\Y #\G)))))


(define (clue/string->clue/int str)
  (for/fold ([n 0])
            ([c (in-string str)])
    (+ (* n 3)
       (case c [(#\G) 0] [(#\Y) 1] [(#\B) 2] [else (error "wrong character")]))))

(define (clue/int->clue/string clue)
  (apply
   string
   (for/list ([c (in-string (~r clue #:base 3 #:min-width word-len #:pad-string "0"))])
     (case c [(#\0) #\G] [(#\1) #\Y] [(#\2) #\B]))))

(define (clue/string->squares clue)
  (apply
   string
   (for/list ([c clue])
     (case c
       [(#\B) #\⬛]
       [(#\Y) #\🟨]
       [(#\G) #\🟩]))))

(define (letter->integer c)
  (- (char->integer c)
     (char->integer #\a)))

;; This is the observation we obtain for the action guess if solution is the real environment.
;; --Memoization does speed up significantly.-- Memoization doesn't seem to help anymore
;;    (au contraire)
;; aka `get-observation`
;; Ternary alphabet: G=0, Y=1, B=2
;; Returns an integer between 0 and 3^5 - 1
(define (guess->clue/int guess solution)
  (define letter-counts (make-vector 26 0))
  (define soll (string->list solution))
  (define greens
    (for/list ([cguess (in-string guess)]
               [csoll (in-list soll)])
      (cond
        [(eq? cguess csoll) 0] ; Green; can use eq? for ascii
        [else
         (define idx (letter->integer csoll))
         (vector-set! letter-counts idx (+ 1 (vector-ref letter-counts idx)))
         #false])))
  ;; 
  (for/fold ([n 0])
            ([cguess (in-string guess)]
             [green (in-list greens)])
    (cond
      [green (* n 3)] ; +0, Green
      [else
       (define idx (letter->integer cguess))
       (define c (vector-ref letter-counts idx))
       (cond [(= c 0) (+ 2 (* n 3))] ; Black
             [else
              (vector-set! letter-counts idx (- c 1))
              (+ 1 (* n 3))])]))) ; Yellow

(module+ test
  (check-equal? (guess->clue/int "those" "those") (clue/string->clue/int "GGGGG"))
  (check-equal? (guess->clue/int "chose" "those") (clue/string->clue/int "BGGGG"))
  (check-equal? (guess->clue/int "thoss" "those") (clue/string->clue/int "GGGGB"))
  (check-equal? (guess->clue/int "tense" "those") (clue/string->clue/int "GBBGG"))
  (check-equal? (guess->clue/int "asset" "those") (clue/string->clue/int "BYBYY"))
  (check-equal? (guess->clue/int "ssset" "those") (clue/string->clue/int "YBBYY")))

;; Then expected size is (∑_i n_i²) / (∑_i n_i)
;; NOTICE: to avoid unnecessary computation, we don't divide by (length goals+),
;; as we assume that goals+ doesn't change in the comparison class.
(define (guess-value guess goals+)
  (define vclues (make-vector n-possible-clues 0)) ; there are only 243 possible clues!

  (define clue-min n-possible-clues) ; use to iterate over vclues only in the segment of interest
  (define clue-max 0)
  (for ([target (in-list goals+)])
    (define clue (guess->clue/int guess target))
    (unless (clue-win? clue) ; consider set size of 0 for winning clue
      (when (< clue clue-min) (set! clue-min clue))
      (when (> clue clue-max) (set! clue-max clue))
      (vector-set! vclues clue (+ 1 (vector-ref vclues clue)))))

  (for/sum ([n (in-vector vclues clue-min (max clue-min (+ 1 clue-max)))])
    (* n n)))

;; Returns the guess among guesses that separates goals+ the best.
;; aka get-best-action
(define (get-best-guess guesses goals+)
  ; Pick an action.
  ; Pick a word to test how good it is at gaining information.
  (for/fold ([best-guess #f]
             [min-max-n-goal++ +inf.0]
             #:result best-guess)
            ([guess guesses])
    ; For each environment, assess the quality of the action.
    ; Assume this is the target goal.
    ; Objective function: Minimize worst number of remaining goals.
    (define max-n-goal++ (guess-value guess goals+))
    (if (< max-n-goal++ min-max-n-goal++)
      (begin (unless (*silent?*) (displayln (list guess (/ max-n-goal++ 1. (length goals+)))))
             (values guess max-n-goal++))
      (begin #;(display "  ")
             #;(displayln (list guess max-n-goal++))
             (values best-guess min-max-n-goal++)))))

(define (get-best-guess* goals+ goals- allowed+ allowed-
                         #:consistent-only? [consistent-only? #f])
  (define words
    ; Note: we always put goals+ first to prefer consistent answers.
    (if consistent-only?
      (in-sequences (in-list goals+)
                    (in-list allowed+))
      (in-sequences (in-list goals+)
                    (in-list goals-)
                    (in-list allowed+)
                    (in-list allowed-))))
  (get-best-guess words goals+))

;; Returns the sublist of words in goals+ that are consistent with the given clue.
;; aka update-environment-posteriors, with binary posteriors
(define (partition-goals+ guess clue goals+)
  (partition (λ (g+) (= clue (guess->clue/int guess g+)))
             goals+))

(define (ask-clue goals+)
  (let loop ()
    (displayln "Enter clue [G=correct, Y=wrong place, B=incorrect]:")
    (define clue (read-line))
    (cond
      [(member clue '("help"))
       (displayln "Commands: list, help")]
      [(member clue '("list"))
       (pretty-print goals+)
       (loop)]
      [(clue/string? clue)
       clue]
      [else
       (loop)])))

(define (ask-guess get-best-guess** goals+)
  (let loop ()
    (display "Make a guess: ")
    (flush-output)
    (define guess (read-line))

    (case guess
      [("help")
       (displayln "Commands: list, best, help")]
      [("list")
       (unless (*silent?*) (pretty-print goals+))
       (loop)]
      [("best")
       (printf "Best guess: ~a\n" (get-best-guess**))
       (loop)]
      [else
       (unless (*silent?*)
         (define qual (guess-value guess goals+))
         (printf "guess value: ~a\n" (/ qual 1. (length goals+))))
       guess])))

;; Assumes that goals is the list of goals in the same order as the official Wordle list.
(define (share-social history goals)
  (cond
    [(history-win? history)
     (define target (first (first history)))
     (printf "Wordle ~a ~a/6~a\n"
             (index-of goals target)
             (length history)
             (if (*consistent-only?*) "*" ""))
     (for-each displayln (map (compose clue/string->squares clue/int->clue/string second)
                              (reverse history)))]
    [else (displayln "Not solved, nothing to share for social media.")]))

(define (history->string h)
  (string-append* (map ~a (flatten h))))

;; If target is #f, then the user is asked for clues.
;; Warning: some globals act as argument. Bad style.
(define (play target goals allowed #:? [history-hash (make-hash)])
  (when (and target (not (member target goals)))
    (error "Word is not authorized:" target))
  
  (let/ec return
    (for/fold ([goals+ goals]
               [goals- '()]
               [allowed+ allowed]
               [allowed- '()]
               [history '()])
              ([i (in-naturals 1)])
      (define (get-best-guess**)
        (hash-ref! history-hash
                   (history->string history)
                   (λ ()
                     (get-best-guess* goals+ goals- allowed+ allowed-
                                      #:consistent-only? (*consistent-only?*)))))
      (unless (*silent?*)
        (printf "\n~a: #goals+: ~a #allowed+: ~a\n" i (length goals+) (length allowed+))
        (flush-output))
      
      (define guess
        (cond [(*auto?*)
               (define guess (get-best-guess**))
               (unless (*silent?*)
                 (printf "Guess: ~a\n" guess))
               guess]
              [else (ask-guess get-best-guess** goals+)]))
      
      (define clue-str
        (if target
          (clue/int->clue/string (guess->clue/int guess target))
          (ask-clue goals+)))
      (define clue (clue/string->clue/int clue-str))
                        
      (unless (*silent?*)
        (if (*squares?*)
          (printf "~a\n~a\n" (apply string (add-between (string->list guess) #\space))
                  (clue/string->squares clue-str))
          (printf "~a\n~a\n" guess (clue/string->squares clue-str))))
           
      (define-values (goals++ goals+-)
        (partition-goals+ guess clue goals+))
      (define-values (allowed++ allowed+-)
        (partition-goals+ guess clue allowed+))
           
      (define new-history (cons (list guess clue) history))
      (cond [(history-win? new-history)
             (unless (*silent?*) (printf "Solved in ~a guesses.\n" i))
             (when (*share-social?*)
               (share-social new-history goals))
             (return (reverse new-history))]
            [else
             (values goals++
                     (append goals+- goals-)
                     allowed++
                     (append allowed+- allowed-)
                     new-history)]))))

(define (sum-guesses occs)
  (for/sum ([(k v) (in-dict occs)])
    (* k v)))

(define (expectation occs)
  (/ (for/sum ([(k v) (in-dict occs)])
       (* k v))
     1.
     (apply + (dict-values occs))))

(define (failures occs)
  (for/sum ([(k v) (in-dict occs)]
            #:when (> k 6))
    v))

(define (print-occs-stats occs)
  (printf "worst-case: ~a\texpectation: ~a\t#guesses: ~a\tfailures: ~a\n"
          (apply max (dict-keys occs))
          (expectation occs)
          (sum-guesses occs)
          (failures occs)))

(define (file->words f)
  (filter-map
   (λ (w) (define s (string-trim w))
     (and (guess? w) w))
   (with-input-from-file f port->lines)))

(module+ main
  (require racket/file)
  
  (define-global *target* #false
    '("The word to guess. If not provided, clues are asked."
      "If 'all', then all words are tried in order, and some statistics are printed."
      "If 'random', a word is chosen at random")
    (λ (s) (or (member s '("all" "random"))
               (= 5 (string-length s))))
    values)

  (define-global *goals-file* "goals.txt"
    "The file containing the list of valid goal words (one per line)."
    file-exists?
    values
    '("--goals" "-g"))
  (define-global *allowed-file* #f
    "The file containing the list of _additional_ valid guesses. Default: none."
    file-exists?
    values
    '("--allowed" "-a"))

  (define-global *cache-file* #f
    '("File where to load (if exists) and save (at the end of the program)" 
      "the optimal actions for faster decisions in similar circumstances.")
    values
    values
    '("--cache"))
  
  (void (globals->command-line))

  (define goals (file->words (*goals-file*)))
  (define allowed (if (*allowed-file*) (file->words (*allowed-file*)) '()))
  (unless (*silent?*)
    (printf "#goals: ~a #allowed: ~a\n" (length goals) (length allowed)))

  (define target (if (equal? (*target*) "random")
                   (random-ref goals)
                   (*target*)))

  ;; Currently it's a flat hash, but a decision tree (hash of hashes...) would be faster.
  (define history-hash
    (if (and (*cache-file*) (file-exists? (*cache-file*)))
      (make-hash (file->value (*cache-file*)))
      (make-hash)))
  (cond [(equal? target "all")
         ; Play all games with all goal words as targets.
         (define occs (make-hash))
         (for ([target (in-list goals)])
           (define res (play target goals allowed #:history-hash history-hash))
           (hash-update! occs (length res) add1 0)
           (printf "~a: ~a\n" target (sort (hash->list occs) < #:key car)))
         (print-occs-stats occs)]
        [else
         ; Play one game with the given target.
         (println (play target goals allowed #:history-hash history-hash))])
  (when (*cache-file*)
    (write-to-file (hash->list history-hash) (*cache-file*) #:exists 'replace)))

#| Some stats
Expected case
------
arise as first word
 time racket wordle-solver.rkt --target all --silent
shave: ((1 . 1) (2 . 46) (3 . 1112) (4 . 1091) (5 . 65))
expectation: 8118

 time racket wordle-solver.rkt --target all --silent --consistent-only
shave: ((1 . 1) (2 . 87) (3 . 983) (4 . 1008) (5 . 185) (6 . 35) (7 . 12) (8 . 4))
expectation: 8407

 time racket wordle-solver.rkt --target all --silent --no-use-allowed
shave: ((1 . 1) (2 . 53) (3 . 1077) (4 . 1089) (5 . 93) (6 . 2))
expectation: 8171

 time racket wordle-solver.rkt --target all --silent --no-use-allowed --consistent-only
shave: ((1 . 1) (2 . 122) (3 . 971) (4 . 947) (5 . 219) (6 . 39) (7 . 12) (8 . 4))
expectation: 8391, fails on 16 words
------
raise as first word
 time racket wordle-solver.rkt --target all --silent
shave: ((1 . 1) (2 . 56) (3 . 1118) (4 . 1078) (5 . 62))
expectation: 8089

 time racket wordle-solver.rkt --target all --silent --consistent-only
shave: ((1 . 1) (2 . 100) (3 . 976) (4 . 994) (5 . 192) (6 . 37) (7 . 12) (8 . 3))
expectation: 8395
 hard words: match, vaunt, hound, all due to being trapped on trying the first letter.

 time racket wordle-solver.rkt --target all --silent --no-use-allowed
shave: ((1 . 1) (2 . 61) (3 . 1073) (4 . 1090) (5 . 90))
expectation: 8152

 time racket wordle-solver.rkt --target all --silent --no-use-allowed --consistent-only
shave: ((1 . 1) (2 . 131) (3 . 957) (4 . 946) (5 . 225) (6 . 41) (7 . 11) (8 . 3))
expectation: 8390, fails on 14 words
----
roate (optimal found in greedy auto mode)
 time racket wordle-solver.rkt --target all --silent
shave: ((2 . 55) (3 . 1130) (4 . 1090) (5 . 40))
worst-case: 5	expectation: 8060

 time racket wordle-solver.rkt --target all --silent --consistent-only
shave: ((2 . 92) (3 . 1015) (4 . 981) (5 . 185) (6 . 34) (7 . 7) (8 . 1))
worst-case: 8	expectation: 8339
(fails only on 8 words instead of 16 for arise!)

/!\ roate is in the allowed list
 time racket wordle-solver.rkt --target all --silent --no-use-allowed
shave: ((2 . 65) (3 . 1085) (4 . 1094) (5 . 71))
worst-case: 5	expectation: 8116

/!\ roate is in the allowed list
 time racket wordle-solver.rkt --target all --silent --no-use-allowed --consistent-only
shave: ((2 . 126) (3 . 1006) (4 . 935) (5 . 196) (6 . 39) (7 . 8) (8 . 3) (9 . 2))
worst-case: 9	expectation: 8322
(better expectation than raise, but worse worst case, but fails on only 13 words)

-----
reast: (not a goal word)
 time racket wordle-solver.rkt --target all --silent
worst-case: 5	expectation: 3.4475161987041036	#guesses: 7981	failures: 0

 time racket wordle-solver.rkt --target all --silent --consistent-only
worst-case: 8	expectation: 3.545572354211663	failures: 9

 time racket wordle-solver.rkt --target all --silent --no-use-allowed
worst-case: 5	expectation: 3.4738660907127428	failures: 0

 time racket wordle-solver.rkt --target all --silent --no-use-allowed --consistent-only
worst-case: 9	expectation: 3.5473002159827214	failures: 11


|#