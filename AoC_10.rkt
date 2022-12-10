;;;AoC_10.rkt
;;;2022-12-10
;;;Mike Quigley

;;;This is too easy...
;;;The input is only 139 lines. I could probably solve this by hand.
;;;What will part 2 bring, I wonder?

;;;Update: Oh, it's one of these puzzles where the output spells out
;;;a word in giant latters. Those are always fun. I'll finish in the morning,
;;;but it looks like my approach to part 1 of making a vector containing
;;;the value of the x register during every cycle will set me up well for part 2
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (cond ((eof-object? line) null)
          ((string=? line "noop\r") (cons "noop" (read-input file)))
          (else (cons
                 (string->number (second (string-split line)))
                 (read-input file))))))

(define (follow-instrs instrs)
  (define (iter instrs x hist)
    (cond ((null? instrs) (cons x hist))
          ((eq? (car instrs) "noop") (iter (cdr instrs) x (cons x hist)))
          (else (iter (cdr instrs)
                      (+ x (car instrs))
                      (cons x (cons x hist))))))
  (list->vector (reverse (iter instrs 1 '(0)))))

(define input-file (open-input-file "Test10-1.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define signals (follow-instrs input))

(display "Part 1: ")
(foldl + 0 (map (λ (x)
                  (* (vector-ref signals x) x))
                '(20 60 100 140 180 220)))