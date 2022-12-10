;;;AoC_09.rkt
;;;2022-12-09
;;;Mike Quigley

;;;Rope physics simulator
;;;Part 1 defines some rules for how a rope of length 1 will behave
;;;So far, I have partially implemented these.
;;;Haven't seen part 2 yet, but it's likely to involve chaining these
;;;length-1 ropes together to form longer ropes.
;;;Update: Part 1 is done, part 2 is exactly what I was expecting

;;;For part 2, it may be best to store a rope in reverse order
;;;So it's a list of 10 coords with the tail of the rope at the head of the list
;;;This makes it easier to iterate over, and faster to get the tail

;;;The original part 1 solution has been replaced using a rope of length 2
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (list (string-ref (first (string-split line)) 0)
                    (string->number (second (string-split line))))
              (read-input file)))))

;;Getters and setters for coord
(define (make-coord x y)
  (cons x y))
(define (get-x coord)
  (car coord))
(define (get-y coord)
  (cdr coord))

;;Are the two coords equal or adjacent?
(define (adjacent? coord-a coord-b)
  (and (<= (abs (- (get-x coord-a) (get-x coord-b))) 1)
       (<= (abs (- (get-y coord-a) (get-y coord-b))) 1)))

;;Generates a new coord, adjacent and offset by 1 in the given direction
;;In addition to U, D, L and R, I am using Q, E, Z, and C for diagonal movement
;;based on their positions on a QWERTY keyboard
(define (move-coord direction coord)
  (cond ((char=? direction #\U) ;Up
         (make-coord (get-x coord) (+ (get-y coord) 1)))
        ((char=? direction #\D) ;Down
         (make-coord (get-x coord) (- (get-y coord) 1)))
        ((char=? direction #\L) ;Left
         (make-coord (- (get-x coord) 1) (get-y coord)))
        ((char=? direction #\R) ;Right
         (make-coord (+ (get-x coord) 1) (get-y coord)))
        ((char=? direction #\Q) ;Up and Left
         (make-coord (- (get-x coord) 1) (+ (get-y coord) 1)))
        ((char=? direction #\E) ;Up and Right
         (make-coord (+ (get-x coord) 1) (+ (get-y coord) 1)))
        ((char=? direction #\Z) ;Down and Left
         (make-coord (- (get-x coord) 1) (- (get-y coord) 1)))
        ((char=? direction #\C) ;Down and Right
         (make-coord (+ (get-x coord) 1) (- (get-y coord) 1)))
        ((char=? direction #\0) ;No movement
         coord)))

;;Returns the direction from tail to head
(define (get-direction head tail)
  (cond ((and (< (get-x head) (get-x tail))
              (> (get-y head) (get-y tail)))
         #\Q)
        ((and (= (get-x head) (get-x tail))
              (> (get-y head) (get-y tail)))
         #\U)
        ((and (> (get-x head) (get-x tail))
              (> (get-y head) (get-y tail)))
         #\E)
        ((and (< (get-x head) (get-x tail))
              (= (get-y head) (get-y tail)))
         #\L)
        ((and (= (get-x head) (get-x tail))
              (= (get-y head) (get-y tail)))
         #\0)
        ((and (> (get-x head) (get-x tail))
              (= (get-y head) (get-y tail)))
         #\R)
        ((and (< (get-x head) (get-x tail))
              (< (get-y head) (get-y tail)))
         #\Z)
        ((and (= (get-x head) (get-x tail))
              (< (get-y head) (get-y tail)))
         #\D)
        ((and (> (get-x head) (get-x tail))
              (< (get-y head) (get-y tail)))
         #\C)))

;;If head and tail are adjacent, return tail unchanged. Otherwise,
;;return new tail coord moved one space towards head
(define (move-tail head tail)
  (if (adjacent? head tail) tail
      (move-coord (get-direction head tail) tail)))

;;Moves the head of rope 1 space in direction
;;returns resulting rope
(define (move-rope direction rope)
  (cond ((null? rope) null)
        ((null? (cdr rope)) (cons (move-coord direction (car rope)) null))
        (else
         (let ((new-rope (move-rope direction (cdr rope))))
           (cons (move-tail (car new-rope) (car rope)) new-rope)))))

;;Move the head of a rope 1 space in any direction
;;Return resulting rope and set of coords visited by tail
(define (follow-instr direction distance rope visited)
  (if (= distance 0) (list rope visited)
      (let ((new-rope (move-rope direction rope)))
        (follow-instr direction
                      (- distance 1)
                      new-rope
                      (set-add visited (car new-rope))))))

;;Follow all instructions from input
;;State is a list containing rope and visited list
(define (follow-instrs instrs state)
  (if (null? instrs) state
      (follow-instrs (cdr instrs)
                     (follow-instr (first (first instrs))
                                   (second (first instrs))
                                   (first state)
                                   (second state)))))


(define input-file (open-input-file "Input09.txt"))
(define input (read-input input-file))
(close-input-port input-file)
(define origin (make-coord 0 0))
(define origin-1 (map (λ (x) origin) (range 2)))
(define origin-2 (map (λ (x) origin) (range 10)))

(display "Part 1: ")
(set-count (second (follow-instrs input (list origin-1 (set)))))

(display "Part 2: ")
(set-count (second (follow-instrs input (list origin-2 (set)))))