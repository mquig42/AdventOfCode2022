;;;AoC_25.rkt
;;;2022-12-25
;;;Mike Quigley

;;;Today, all we need to do is add up a list of numbers.
;;;These numbers are base 5, and some digits have negative value.
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

(foldl + 0 (map snafu->decimal (file->lines "Test25.txt")))
