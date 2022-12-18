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

;;;Update: I've been doing this completely wrong. Don't use a set of points.
;;;This is a situation where a grid of nested lists are better. That gives
;;;stack-like behaviour. It also makes it easy to select a given y-level for
;;;collision detection. Set intersections would work too, but you'd need to
;;;iterate through the whole set.

#lang racket

(define (read-input file)
  (map (λ (x) (if (eq? x #\<) -1 1))
       (string->list (string-trim (read-line file)))))

(define (make-point x y)
  (cons x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cdr point))
(define (point-add a b)
  (make-point (+ (get-x a) (get-x b))
              (+ (get-y a) (get-y b))))

;;I have a feeling I'll be using this a lot
(define (inc-mod x m)
  (modulo (+ x 1) m))

;;Makes a set of points
(define (rock->set rock offset)
  (list->set (map (λ (r) (point-add r offset)) rock)))

;;Checks if a rock is outside the walls
(define (oob? rock x-offset)
  (or (< (+ (get-x (first rock)) x-offset) 0)
      (> (+ (get-x (second rock)) x-offset) 6)))

;;stack is a set of points. Returns maximum y value of those points
;;Might not want to use this. It takes linear time
(define (max-height stack)
  (argmax identity (map get-y (set->list stack))))

(define input-file (open-input-file "Test17.txt"))
(define input (read-input input-file))
(close-input-port input-file)

;;Hard-code the shapes and put them in a list
;;Each rock has its minimum x-value in the first point and the max in the second
;;The first x value is always 2 so x offsets can start at 0
(define r1 '((2 . 0) (5 . 0) (3 . 0) (4 . 0)))
(define r2 '((2 . 1) (4 . 1) (3 . 0) (3 . 1) (3 . 2)))
(define r3 '((2 . 0) (4 . 0) (3 . 0) (4 . 1) (4 . 2)))
(define r4 '((2 . 0) (2 . 1) (2 . 2) (2 . 3)))
(define r5 '((2 . 0) (3 . 0) (2 . 1) (3 . 1)))
(define rocks (list r1 r2 r3 r4 r5))

(define cave-floor
  (set '(0 . 0) '(1 . 0) '(2 . 0) '(3 . 0) '(4 . 0) '(5 . 0) '(6 . 0)))

