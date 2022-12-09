;;;AoC_08.rkt
;;;2022-12-08
;;;Mike Quigley

;;;This was a fun one to solve. It's all based on comparing values in a
;;;heightmap to see which points are visible from off the edge, and
;;;the view distance in each direction from each point.
;;;Need to iterate over the data a lot.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (map char->number (string->list (string-trim line)))
              (read-input file)))))

;;Converts a character to an integer representing its value
;;Note: not ASCII code. the char #\7 evaluates to the integer value 7
(define (char->number c)
  (- (char->integer c) (char->integer #\0)))

;;getter for tree height
(define (get-height trees row col)
  (list-ref (list-ref trees row) col))

;;gets a row of trees as a list
(define (get-row trees row)
  (list-ref trees row))

;;gets a column of trees as a list
(define (get-col trees col)
  (map (λ (x) (list-ref x col)) trees))

;;Maximum value of a list
(define (lst-max lst)
  (if (null? lst) -1
      (max (car lst) (lst-max (cdr lst)))))

;;Gets the number of elements in lst before tgt value is exceeded
(define (lst-dst lst tgt)
  (define (iter lst acc)
    (cond ((null? lst) acc)
          ((>= (car lst) tgt) (+ acc 1))
          (else (iter (cdr lst) (+ acc 1)))))
  (iter lst 0))

;;Is the tree at the given coords visible from any direction?
(define (visible? trees row col)
  (let* ((height (get-height trees row col))
         (row-lst (get-row trees row))
         (col-lst (get-col trees col))
         (left (take row-lst col))
         (right (drop row-lst (+ col 1)))
         (up (take col-lst row))
         (down (drop col-lst (+ row 1))))
    (or (> height (lst-max left))
        (> height (lst-max right))
        (> height (lst-max up))
        (> height (lst-max down)))))

;;Calculates a scenic score by multiplying the view distance in each direction
(define (scenic-score trees row col)
  (let* ((height (get-height trees row col))
         (row-lst (get-row trees row))
         (col-lst (get-col trees col))
         (left (reverse (take row-lst col)))
         (right (drop row-lst (+ col 1)))
         (up (reverse (take col-lst row)))
         (down (drop col-lst (+ row 1))))
    (* (lst-dst left height)
       (lst-dst right height)
       (lst-dst up height)
       (lst-dst down height))))

;;This can be used to map the visible? and scenic-score functions over
;;the full range of (row, col) coordinates
(define (grid-map proc trees)
  (map (λ (row)
         (map (λ (col)
                (proc input row col))
              (range (length (car trees)))))
       (range (length trees))))


(define input-file (open-input-file "Input08.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(count identity (flatten (grid-map visible? input)))
(display "Part 2: ")
(argmax identity (flatten (grid-map scenic-score input)))

(display "\r=== Map of Visible Trees ===\r")
(map (λ (line) (list->string (map (λ (x) (if x #\▲ #\.)) line))) (grid-map visible? input))
