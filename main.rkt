#lang racket/base
(require racket/dict
         racket/list
         racket/pretty
         racket/string
         racket/file
         racket/format
         racket/port
         racket/random
         define2
         global)

;;; Author: Laurent Orseau https://github.com/Metaxal
;;; License: [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) or
;;;          [MIT license](http://opensource.org/licenses/MIT) at your option.

;;; See the README: https://github.com/Metaxal/wordle-solver#readme

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

(define-global:string *first-word* #f
  "First word to play, to speed up computation.")

;;; A clue/int is a number between 0 and 3^5, and corresponds to the sequence of black, yellow and
;;; green squares (clue/string).
;;; Greedy goal: minimize the size of the remaining set in the worst case

(define word-len 5)
(define n-possible-clues (expt 3 word-len))

(define clue-win 0) ; 0 is "GGGGG" in integer encoding
(define (clue-win? c) (= clue-win c))

(define (history-win? history)
  (clue-win? (second (first history))))

(define (guess? str)
  (and (= word-len (string-length str))
       ;; Two characters are eqv? if they correspond to the same scalar value.
       ;; For each scalar value less than 256, character values that are eqv? are also eq?.
       (andmap (λ (x) (< (char->integer x) 256))
               (string->list str))
       ; We allow only alphabetic lowercase because we use a vector of 26 letters
       #;
       (andmap (λ (x) (and (char-alphabetic? x)
                           (char-lower-case? x)))
               (string->list str))))

(define (clue/int? x)
  (and (exact-nonnegative-integer? x)
       (< x n-possible-clues)))

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
;;
;; The function works only for lower-case alphabetic characters.
#;
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

;; This function works for all ascii characters (<256), using a hasheq instead of a vector,
;; but is twice as slow.
(define (guess->clue/int guess solution)
  (define letter-counts (make-hasheq)) ; Notice: only for ascii!
  (define greens
    (for/list ([cguess (in-string guess)]
               [csoll (in-string solution)])
      (cond
        [(eq? cguess csoll) 0] ; Green; can use eq? for ascii
        [else
         (hash-update! letter-counts csoll add1 0)
         #false])))
  ;; 
  (for/fold ([n 0])
            ([cguess (in-string guess)]
             [green (in-list greens)])
    (cond
      [green (* n 3)] ; +0, Green
      [else
       (define c (hash-ref letter-counts cguess 0))
       (cond [(= c 0) (+ 2 (* n 3))] ; Black
             [else
              (hash-update! letter-counts cguess sub1)
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
(define (guess-value/expected-size guess goals+)
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

;; Maximum size of the 
(define (guess-value/max-size guess goals+)
  (define vclues (make-vector n-possible-clues 0)) ; there are only 243 possible clues!

  (define clue-min n-possible-clues) ; use to iterate over vclues only in the segment of interest
  (define clue-max 0)
  (for ([target (in-list goals+)])
    (define clue (guess->clue/int guess target))
    (unless (clue-win? clue) ; consider set size of 0 for winning clue
      (when (< clue clue-min) (set! clue-min clue))
      (when (> clue clue-max) (set! clue-max clue))
      (vector-set! vclues clue (+ 1 (vector-ref vclues clue)))))

  (for/fold ([v -inf.0])
            ([n (in-vector vclues clue-min (max clue-min (+ 1 clue-max)))])
    (max v n)))

; Negative entropy of the remaining words
(define (guess-value/negentropy guess goals+)
  (define vclues (make-vector n-possible-clues 0)) ; there are only 243 possible clues!

  (define clue-min n-possible-clues) ; use to iterate over vclues only in the segment of interest
  (define clue-max 0)
  (for ([target (in-list goals+)])
    (define clue (guess->clue/int guess target))
    (vector-set! vclues clue (+ 1 (vector-ref vclues clue))))

  (for/sum ([n (in-vector vclues clue-min (max clue-min (+ 1 clue-max)))])
    (if (= 0 n) 0 (* n (- (log n) (log (length goals+)))))))

(define guess-value
  #;guess-value/max-size
  guess-value/expected-size
  #;guess-value/negentropy)

;; Returns the guess among guesses that minimizes guess-value, i.e.,
;; that separates goals+ the best.
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
       (displayln "Commands: list, help")
       (loop)]
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
       (displayln "Commands: list, best, help")
       (loop)]
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
    (error "Word is not a possible target:" target))
  
  (let/ec return
    (for/fold ([goals+ goals]
               [goals- '()]
               [allowed+ allowed]
               [allowed- '()]
               [history '()])
              ([i (in-naturals 1)])
      (define (get-best-guess**)
        (if (and (empty? history) (*first-word*))
          (*first-word*)
          (hash-ref! history-hash
                     (history->string history)
                     (λ ()
                       (get-best-guess* goals+ goals- allowed+ allowed-
                                        #:consistent-only? (*consistent-only?*))))))
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

(define (main #:! play)
 
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

(module+ main (main #:play play))
