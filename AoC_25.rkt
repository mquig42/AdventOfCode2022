;;;AoC_25.rkt
;;;2022-12-25
;;;Mike Quigley

;;;Today, all we need to do is add up a list of numbers.
;;;These numbers are base 5, and some digits have negative value.

;;;Update: I figured out an algorithm for converting a number n to SNAFU
;;;First, get the length. If "10000" is < n and "20000" is > n, then
;;;the length must be 5. If they're both < n, add another 0.
;;;Now, generate a string of that length that's all '=' symbols. This will be
;;;the smallest possible number of that length, so almost certainly too small.
;;;Replace the first digit with, in sequence, -, 0, 1, 2. If the SNAFU value
;;;of the string is > n, you've gone too far so step back one.
;;;For example, if "1====" < n and "2====" > n, then the first digit must be 1.
;;;Or, to rephrase: find the largest digit that still makes the string < n.
;;;Once you figure out the value of a digit, move on to the next one. Keep going
;;;until you find a string that evaluates to n.

;;;I've done this by hand to get the star. TODO make the program do it.
#lang racket

;;Decodes a SNAFU number given as a string
(define (snafu->decimal v)
  (define (iter lst place acc)
    (if (null? lst) acc
        (iter (cdr lst)
              (* place 5)
              (+ acc (* place (s-digit->dec (car lst)))))))
  (iter (reverse (string->list v)) 1 0))

;;Decimal value of a SNAFU digit, given as char
(define (s-digit->dec c)
  (cond ((eq? c #\2) 2)
        ((eq? c #\1) 1)
        ((eq? c #\0) 0)
        ((eq? c #\-) -1)
        ((eq? c #\=) -2)))

(foldl + 0 (map snafu->decimal (file->lines "Input25.txt")))
