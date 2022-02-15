# Wordle solver

A simple greedy solver for [Wordle](https://www.powerlanguage.co.uk/wordle/).

## Quick start

Start with:
```shell
racket -l- wordle-solver --help
```
to see the available flags.

**Remark**:
The original Wordle list is not provided for licensing reasons. The program 
assumes that the file `goals.txt` in the current directory contains the list 
of target words to guess (one word per line, each word must exactly 5 a-z 
characters). This default can be changed with the `--goals` flag. To use an 
additional list of allowed words that are not target words, use the 
`--allowed` flag. 

#### Play the game yourself:
```shell
racket -l- wordle-solver --target random --no-auto
```

Use the flag `--share-social` to print a summary like Wordle does.

#### Make the solver guess a random target word:
```shell
racket -l- wordle-solver --target random 
```
Use `--target <word>` to make it guess a particular word that is assumed to be in `goals.txt`.

The first guess is a little slow to compute (a few seconds), but because it 
is always the same it can be cached. Use the flag `--cache <cache-file>` to 
load (if the file exists) and save cached results. The cache file should be made 
specific to the list of goals, the list of allowed words and whether 
`--consistent-only` is used.

Use the flag `--consistent-only` to make the solver play in 'very hard mode', that is,
any word played must be consistent with all the clues obtained so far.

#### Make the solver guess all words from `goals.txt` in sequence:
```shell
racket -l- wordle-solver --target all --silent
```
Observe how the solver speeds up as more results get cached. The cache can be saved to disk
using the `--cache <file>` to be reused in a later invokation of the program (with the same flag).

#### Use the solver as an assistant (just omit the `--target` flag):
```shell
racket -l- wordle-solver 
```
The program proposes words and the user enters the clues.


## Guess value

The value of a guess is the expected size of the remaining words
after the next clue is observed.

While this greedy strategy is not optimal (an optimal strategy requires planning),
in particular with `--hard-mode` and `--consistent-only`,
it is still quite good in general especially given how simple it is.

On the original Wordle lists of 2315 solution words and 10657 allowed words, it has an expected 
number of guesses of 3.48 (8060/2315, first word: `roate`) while [the optimum is 3.42](http://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle#Assumptions_about_the_rules_of_Wordle) 
(see also this [blog post](https://www.poirrier.ca/notes/wordle-optimal/)), with a worst case of 
5 guesses at most. By forcing the first word to be `salet`, the average number of guesses is 
3.448 (7982/2135) with a single word taking 6 guesses.


With `--consistent-only`, it uses 3.60 (8339/2315) guesses on average, with 7 words requiring 7 guesses
and 1 word requiring 8 guesses. (The same strategy can also be used for `--hard-mode`.)
By forcing the first word to be `salet`, it requires 3.54 (8204/2315) guesses on average,
with one word taking 8 guesses and 6 words taking 7 guesses.

With `--hard-mode`, it takes 3.59 (8325/2315, first word: `roate`) guesses on average, with 4 words taking 7 guesses 
and 1 word taking 8 guesses.
By forcing the first word to be `salet`, it takes 3.54 (8204/2135) guesses on average, with 6 words taking 7 guesses and 1 word taking 8 guesses.



On the whole list of 12972 words, this strategy takes 4.12 guesses on average (53503/12972),
with a worst case of 8 (only 1 word), and 25 words taking more than 6 guesses.
