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
load (if the file exists) and save cached results. The cache file should made 
be specific to the list of goals, the list of allowed words and whether 
`--consistent-only` is used.

Use the flag `--consistent-only` to make the solver play in 'hard mode', that is,
any word played must be consistent with all the clues obtained so far.

#### Make the solver guess all words from `goals.txt` in sequence:
```shell
racket -l- wordle-solver --target all --silent
```
Observe how the solver speed up as more results get cached. The chac

#### Use the solver as an assistant (just omit the `--target` flag):
```shell
racket -l- wordle-solver 
```
The program proposes words and the user enters the clues.


## Guess value

The value of a guess is the expected size of the remaining words
after the next clue is observed.

While this greedy strategy is not optimal (an optimal strategy requires planning),
in particular in 'hard mode' (flag `--consistent-only`),
it is still quite good in general.

On the Wordle list of original words and allowed words,
it has an expected number of guesses of 3.48 while [the optimum
is 3.42](http://sonorouschocolate.com/notes/index.php?title=The_best_strategies_for_Wordle#Assumptions_about_the_rules_of_Wordle)
(see also this [blog post](https://www.poirrier.ca/notes/wordle-optimal/)).

