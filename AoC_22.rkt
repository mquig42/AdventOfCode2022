;;;AoC_22.rkt
;;;2022-12-22
;;;Mike Quigley

;;;This looks like a difficult one. I see 2 main challenges here:
;;;1. represent the map in a way that allows wrapping
;;;2. Read and follow the instructions

;;;Reading the instructions was easier than I thought, thanks to regexes
;;;I'll store the direction (R or L) as an interned symbol instead of a string.
;;;That way I can use eq? to see which one it is, should be faster than string=?

;;;Simple way of representing the board: a list of two sets, one for open tiles
;;;and one for walls. Direction is a coord pair representing a unit vector
;;;Using the same row/column coordinate system as the problem description,
;;;E = (0 . 1)
;;;S = (1 . 0)
;;;W = (0 . -1)
;;;N = (-1 . 0)
#lang racket

;;Reads one row of input
(define (read-row row row-num tiles walls)
  (define (iter lst col-num tiles walls)
    (cond ((null? lst) (list tiles walls))
          ((eq? (car lst) #\space)
           (iter (cdr lst) (+ col-num 1) tiles walls))
          ((eq? (car lst) #\.)
           (iter (cdr lst) (+ col-num 1)
                 (set-add tiles (make-coord row-num col-num)) walls))
          ((eq? (car lst) #\#)
           (iter (cdr lst) (+ col-num 1)
                 tiles (set-add walls (make-coord row-num col-num))))))
  (iter (string->list row) 1 tiles walls))

;;Reads entire board from file
(define (read-board file row-num tiles walls)
  (let ((line (string-trim (read-line file) #:left? #f)))
    (if (string=? line "") (list tiles walls)
        (let ((parsed-row (read-row line row-num tiles walls)))
          (read-board file
                      (+ row-num 1)
                      (first parsed-row)
                      (second parsed-row))))))

;;Coord get/set
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))
(define (add-coords a b)
  (make-coord (+ (get-row a) (get-row b)) (+ (get-col a) (get-col b))))

;;List of the four cardinal directions
(define faces '((0 . 1) (1 . 0) (0 . -1) (-1 . 0)))

;;Rotates a direction by 90 degrees left or right
(define (turn face dir)
  (let ((op (if (eq? dir 'R) + -)))
    (list-ref faces (modulo (op (index-of faces face) 1) 4))))

;;Rotates a direction by 180 degrees
(define (u-turn face)
  (make-coord (* (get-row face) -1) (* (get-col face) -1)))

(define input-file (open-input-file "Test22.txt"))
(define board (read-board input-file 1 (set) (set)))
(define instrs (map (λ (x) (if (string->number x)
                               (string->number x)
                               (string->symbol x)))
                    (regexp-match* #px"\\d+|\\D"
                                   (string-trim (read-line input-file)))))
(close-input-port input-file)

