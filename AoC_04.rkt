;;;AoC_04.rkt
;;;2022-12-04
;;;Mike Quigley

;;;Today we learned that elves are really bad at making work plans.
;;;The plans, in this case, are pairs of ranges. We need to find out
;;;how many of these pairs fully or partially overlap.

;;;I misunderstood the second part, and spent a bunch of time figuring out how
;;;to check each range for overlap with every other range, not just the one
;;;it's paired with. Might want to go back and actually solve that. It's an
;;;interesting question and my first attempt double-counted a lot of cases.
;;;I'll probably find out that they all overlap. Even just looking at pairs
;;;it was 847 out of 1000. Like I said, whatever elf came up with this work
;;;plan wasn't very good at organizing this sort of thing.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (process-line line) (read-input file)))))

(define (process-line line)
  (map (λ (x) (map string->number (string-split x "-")))
       (string-split (string-trim line) ",")))

(define (partner-full-overlap? p)
  (or (and (>= (first (first p)) (first (second p)))
           (<= (second (first p)) (second (second p))))
      (and (>= (first (second p)) (first (first p)))
           (<= (second (second p)) (second (first p))))))

(define (partner-overlap? p)
  (or (and (>= (second (first p)) (first (second p)))
           (<= (first (first p)) (second (second p))))
      (and (>= (second (second p)) (first (first p)))
           (<= (first (second p)) (second (first p))))))

(define input-file (open-input-file "Input04.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(count identity (map partner-full-overlap? input))
(display "Part 2: ")
(count identity (map partner-overlap? input))
