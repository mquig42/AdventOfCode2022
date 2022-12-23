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

;;;For part 2, the wrapping rules are different. Need to figure out how to
;;;turn my input into a cube. Also, the sample and full inputs are different
;;;shapes, so if I want to work with both I need a general solution for folding
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

;;Getters for board.
;;Is the given coord an open tile?
(define (tile? board coord)
  (set-member? (first board) coord))
;;Is the given coord a wall?
(define (wall? board coord)
  (set-member? (second board) coord))

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

(define (find-start-pos board)
  (define (iter pos)
    (if (or (tile? board pos) (wall? board pos)) pos
        (iter (add-coords pos '(0 . 1)))))
  (iter (make-coord 1 1)))

;;Gets next coord in given direction. Wraps on edges.
(define (next-pos-1 board pos face)
  (define (to-edge pos face)
    (let ((ahead (add-coords pos face)))
      (if (or (tile? board ahead) (wall? board ahead))
          (to-edge ahead face)
          (list pos (u-turn face)))))
  (let ((ahead (add-coords pos face)))
    (if (or (tile? board ahead) (wall? board ahead))
        (list ahead face)
        (to-edge pos (u-turn face)))))

;;Move n spaces in facing direction
(define (move next-pos n board pos face)
  (let ((ahead (next-pos board pos face)))
    (if (or (= n 0) (wall? board (first ahead))) (list pos (second ahead))
        (move next-pos (- n 1) board (first ahead) (second ahead)))))

;;Gets position and heading after all moves
(define (move-seq next-pos lst board pos face)
  (cond ((null? lst) (list pos face))
        ((number? (car lst))
         (let ((ahead (move next-pos (car lst) board pos face)))
           (move-seq next-pos (cdr lst) board (first ahead) (second ahead))))
        (else
         (move-seq next-pos (cdr lst) board pos (turn face (car lst))))))

;;Prints ending position and resulting password
(define (print-result end-state)
  (printf "Final position: ~a Heading: ~a\r"
          (first end-state)
          (index-of faces (second end-state)))
  (display "Password: ")
  (+ (* 1000 (get-row (first end-state)))
     (* 4 (get-col (first end-state)))
     (index-of faces (second end-state))))

(define input-file (open-input-file "Input22.txt"))
(define board (read-board input-file 1 (set) (set)))
(define instrs (map (λ (x) (if (string->number x)
                               (string->number x)
                               (string->symbol x)))
                    (regexp-match* #px"\\d+|\\D"
                                   (string-trim (read-line input-file)))))
(close-input-port input-file)

(display "Part 1:\r")
(print-result
 (move-seq next-pos-1 instrs board (find-start-pos board) '(0 . 1)))

