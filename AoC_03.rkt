;;;AoC_03.rkt
;;;2022-12-03
;;;Mike Quigley

;;;Today's theme is checking for common characters between strings.
;;;Not much to say about my solution. Each function is pretty small
;;;and straightforward.
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

;;Returns the first member of list-a that is also in list-b
;;or #f if no match found
(define (find-dup lst-a lst-b)
  (cond ((null? lst-a) #f)
        ((member (car lst-a) lst-b) (car lst-a))
        (else (find-dup (cdr lst-a) lst-b))))

;;Same as above, but with three lists
(define (find-dup3 lst-a lst-b lst-c)
  (cond ((null? lst-a) #f)
        ((and (member (car lst-a) lst-b) (member (car lst-a) lst-c))
         (car lst-a))
        (else (find-dup3 (cdr lst-a) lst-b lst-c))))

;;Part 1 solution: Finds the priority level of the duplicate item in a given
;;rucksack
(define (prioritize-rucksack r)
  (let ((splitter (/ (length r) 2)))
    (char->priority (find-dup (take r splitter) (drop r splitter)))))

;;Finds solution for part 2
(define (solve2 input)
  (if (null? input) 0
      (+ (char->priority
          (find-dup3 (first input) (second input) (third input)))
         (solve2 (drop input 3)))))

(define input-file (open-input-file "Input03.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl + 0 (map prioritize-rucksack input))
(display "Part 2: ")
(solve2 input)
