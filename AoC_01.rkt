;;;AoC_01.rkt
;;;2022-12-01
;;;Mike Quigley

;;;OK, this year's theme is a jungle expedition. To start with, we're assessing
;;;our food supplies. The input is a list of food items grouped by carrier elf,
;;;and each elf is separated by a blank line. Need to find total calories
;;;for each elf.
#lang racket

(define (sum-calories file)
  (define (iter acc cals)
    (let ((line (read-line file)))
      (cond ((eof-object? line) (cons acc cals))
            ((string=? "\r" line)
             (iter 0 (cons acc cals)))
            (else
             (iter (+ acc (string->number (string-trim line))) cals)))))
  (iter 0 null))


(define input-file (open-input-file "Input01.txt"))
(define totals (sort (sum-calories input-file) >))
(close-input-port input-file)

(display "Part 1: ")
(car totals)
(display "Part 2: ")
(foldl + 0 (take totals 3))
