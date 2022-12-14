;;;AoC_13.rkt
;;;2022-12-13
;;;Mike Quigley

;;;Today is all about lists. Reading input was straightforward, just get rid of
;;;the commas and let the LISP parser do the rest of the work.

;;;Part 1 was writing a compare function.
;;;For part 2, all I need to do is sort the input. Easy.
#lang racket

(define (read-input file)
  (let ((line1 (read-line file))
        (line2 (read-line file))
        (line3 (read-line file)))
    (if (eof-object? line1) null
        (cons (list (parse-line line1) (parse-line line2))
              (read-input file)))))

;;Convert a single line of input into a list of (lists of) integers
(define (parse-line line)
  (call-with-input-string (string-replace line "," " ") read))

;;Returns 1 of left and right are in order, 0 if they aren't,
;;and -1 if they're equal
(define (compare left right)
  (cond ((and (number? left) (number? right) (< left right)) 1)
        ((and (number? left) (number? right) (> left right)) 0)
        ((and (number? left) (number? right) (= left right)) -1)
        ((and (null? left) (null? right)) -1)
        ((and (null? left) (list? right)) 1)
        ((and (list? left) (null? right)) 0)
        ((and (number? left) (list? right)) (compare (list left) right))
        ((and (list? left) (number? right)) (compare left (list right)))
        (else ;Both lists
         (let ((cmp (compare (car left) (car right))))
           (if (= cmp -1) (compare (cdr left) (cdr right))
               cmp)))))

;;Predicate version of compare function. Returns true if left < right
(define (compare? left right)
  (= (compare left right) 1))

;;Inserts dividers, sorts all packets, returns product of the divider indices
(define (solve2 lst)
  (let ((sorted (sort (cons '((2)) (cons '((6)) lst)) compare?)))
    (* (+ (index-of sorted '((2))) 1) (+ (index-of sorted '((6))) 1))))

;;Splits each pair. Returns a list of all packets
(define (split-pairs lst)
  (if (null? lst) null
      (cons (first (car lst))
            (cons (second (car lst))
                  (split-pairs (cdr lst))))))


(define input-file (open-input-file "Input13.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl + 0 (map (λ (a b) (* a b))
                (map (λ (x) (compare (first x) (second x))) input)
                (range 1 (+ (length input) 1))))

(display "Part 2: ")
(solve2 (split-pairs input))
