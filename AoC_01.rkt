;;;AoC_01.rkt
;;;2022-12-01
;;;Mike Quigley

;;;OK, this year's theme is a jungle expedition. To start with, we're assessing
;;;our food supplies. The input is a list of food items grouped by carrier elf,
;;;and each elf is separated by a blank line. Need to find total calories
;;;for each elf.
#lang racket

;;TODO: There's no need to iterate through the input twice.
;;Combine these functions so sum-calories works directly with the file
(define (read-lines file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string->number (string-trim line))
              (read-lines file)))))

(define (sum-calories lst)
  (define (iter acc cals lst)
    (cond ((null? lst) (cons acc cals))
          ((number? (car lst))
           (iter (+ acc (car lst)) cals (cdr lst)))
          (else
           (iter 0 (cons acc cals) (cdr lst)))))
  (iter 0 null lst))


(define input-file (open-input-file "Input1.txt"))
(define input (read-lines input-file))
(close-input-port input-file)

(define totals (sum-calories input))
(display "Part 1: ")
(argmax identity totals)
(display "Part 2: ")
(foldl + 0 (take (sort totals >) 3))
