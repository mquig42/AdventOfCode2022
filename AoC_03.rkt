;;;AoC_03.rkt
;;;2022-12-03
;;;Mike Quigley

;;;Today's theme is checking for common characters between strings.
;;;Not much to say about my solution. Each function is pretty small
;;;and straightforward.

;;;UPDATE: Made a variable-arity find-dup function that uses sets.
;;;This replaces the original find-dup and find-dup3
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string->list (string-trim line)) (read-input file)))))

;;Convert a char to a priority level.
;;Lowercase letters a->z have priority 1->26
;;uppercase letters A->Z have priority 27->52
(define (char->priority c)
  (if (char-lower-case? c)
      (+ (- (char->integer c) (char->integer #\a)) 1)
      (+ (- (char->integer c) (char->integer #\A)) 27)))

;;Use set intersections to find the common element in any number of lists
(define (find-dup . lsts)
  (set-first (apply set-intersect (map list->set lsts))))

;;Part 1 solution: Finds the priority level of the duplicate item in a given
;;rucksack
(define (prioritize-rucksack r)
  (let ((splitter (/ (length r) 2)))
    (char->priority (find-dup (take r splitter) (drop r splitter)))))

;;Finds solution for part 2
(define (solve2 input)
  (if (null? input) 0
      (+ (char->priority
          (find-dup (first input) (second input) (third input)))
         (solve2 (drop input 3)))))

(define input-file (open-input-file "Input03.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl + 0 (map prioritize-rucksack input))
(display "Part 2: ")
(solve2 input)
