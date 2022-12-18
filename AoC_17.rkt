;;;AoC_17.rkt
;;;2022-12-17
;;;Mike Quigley

;;;I cheated and went to the subreddit before even starting this.
;;;This means I know that part 2 is one of those "run part 1 a zillion times"
;;;scenarios.

;;;Could be a repeating pattern. There are 5 rock types, and my input length
;;;is 10091. Both of those are prime numbers. Interestingly, the sample length
;;;is 40, which is not prime

;;;I'll probably need to do the actual simulation as a basis, then extrapolate
;;;results.

;;;For coordinates, (0 . 0) represents the bottom-left of the chamber

#lang racket

(define (read-input file)
  (string->list (string-trim (read-line file))))

(define (make-point x y)
  (cons x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cdr point))
(define (point-add a b)
  (make-point (+ (get-x a) (get-x b))
              (+ (get-y a) (get-y b))))



(define input-file (open-input-file "Input17.txt"))
(define input (read-input input-file))
(close-input-port input-file)

;;Hard-code the shapes and put them in a list
(define r1 '((2 . 0) (3 . 0) (4 . 0) (5 . 0)))
(define r2 '((3 . 0) (2 . 1) (3 . 1) (4 . 1) (3 . 2)))
(define r3 '((2 . 0) (3 . 0) (4 . 0) (4 . 1) (4 . 2)))
(define r4 '((2 . 0) (2 . 1) (2 . 2) (2 . 3)))
(define r5 '((2 . 0) (2 . 1) (3 . 0) (3 . 1)))
(define rocks '(r1 r2 r3 r4 r5))
