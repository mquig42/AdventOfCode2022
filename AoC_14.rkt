;;;AoC_14.rkt
;;;2022-12-14
;;;Mike Quigley

;;;Now, this one could get interesting. And all I can see right now is part 1.
;;;I'll need to generate a map based on the input.
;;;Represent it as a set of points. Each point is a list of two numbers '(x y)
;;;I'll also need to enumerate a list of all the points between two endpoints.
;;;Generating the initial state was actually more work than simulating the sand
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-line (string-trim line)) (read-input file)))))

(define (parse-line line)
  (map (λ (x) (map string->number (string-split x ",")))
       (string-split line " -> ")))

;;A point is a list containing the x and y values
(define (make-point x y)
  (list x y))
(define (get-x point)
  (first point))
(define (get-y point)
  (second point))

;;Not sure this is needed, I just wanted to get an idea what size the map is
;;Gets minimum and maximum x and y values
(define (get-extents lst)
  (list (get-x (argmin get-x (map (λ (x) (argmin get-x x)) lst)))
        (get-x (argmax get-x (map (λ (x) (argmax get-x x)) lst)))
        (get-y (argmin get-y (map (λ (x) (argmin get-y x)) lst)))
        (get-y (argmax get-y (map (λ (x) (argmax get-y x)) lst)))))

;;Finds floor level. This is two spaces below the maximum y-value in input
(define (get-floor-level lst)
  (+ 1 (get-y (argmax second (map (λ (x) (argmax second x)) lst)))))

;;Returns a set of points from start to end inclusive
(define (enumerate-points start end)
  (define (iter-x x-start x-end y acc)
    (if (> x-start x-end) acc
        (iter-x (+ x-start 1) x-end y (set-add acc (make-point x-start y)))))
  (define (iter-y x y-start y-end acc)
    (if (> y-start y-end) acc
        (iter-y x (+ y-start 1) y-end (set-add acc (make-point x y-start)))))
  (cond ((< (get-x start) (get-x end))
         (iter-x (get-x start) (get-x end) (get-y start) (set)))
        ((> (get-x start) (get-x end))
         (iter-x (get-x end) (get-x start) (get-y start) (set)))
        ((< (get-y start) (get-y end))
         (iter-y (get-x start) (get-y start) (get-y end) (set)))
        ((> (get-y start) (get-y end))
         (iter-y (get-x start) (get-y end) (get-y start) (set)))
        (else ;If start and end are equal
         (set start))))

;;Calls enumerate-points over a list of points instead of just 2
(define (enumerate-path lst)
  (define (iter lst acc)
    (if (null? (cdr lst)) acc
        (iter (cdr lst)
              (set-union acc (enumerate-points (first lst) (second lst))))))
  (iter lst (set)))

;;Makes a set of all stone points defined by input
(define (enumerate-all-stones input)
  (foldl set-union (set)
         (map enumerate-path input)))

;;Returns final resting place of a unit of snad dropped from (500 0)
;;y value is capped at floor
(define (sand-endpoint state floor)
  (define (iter x y)
    (cond ((= y floor) (make-point x y))
          ((not (set-member? state (make-point x (+ y 1))))
           (iter x (+ y 1)))
          ((not (set-member? state (make-point (- x 1) (+ y 1))))
           (iter (- x 1) (+ y 1)))
          ((not (set-member? state (make-point (+ x 1) (+ y 1))))
           (iter (+ x 1) (+ y 1)))
          (else (make-point x y))))
  (iter 500 0))

;;How many sand units fall before one of them stops at a y value of level?
(define (time-to-fill state floor level)
  (define (iter state n)
    (let ((sand-point (sand-endpoint state floor)))
      (if (= level (get-y sand-point)) n
          (iter (set-add state sand-point) (+ n 1)))))
  (iter state 0))

(define input-file (open-input-file "Input14.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define initial-state (enumerate-all-stones input))

(display "Part 1: ")
(time-to-fill initial-state (get-floor-level input) (get-floor-level input))
(display "Part 2: ")
;Parts 1 and 2 are actually asking slightly different things
;1. How much sand must drop so that the *next* unit lands on the floor
;2. How much sand must drop so that the *last* unit lands at y=0
;So add 1 before printing answer
(+ 1 (time-to-fill initial-state (get-floor-level input) 0))
