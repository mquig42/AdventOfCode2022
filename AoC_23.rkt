;;;AoC_23.rkt
;;;2022-12-23
;;;Mike Quigley

;;;Looks like it's Cellular Automaton time
;;;Starting this late because yesterday's cube of doom took so long
#lang racket

;;Input is similar to yesterday, but simpler
(define (read-row row row-num elves)
  (define (iter lst col-num elves)
    (cond ((null? lst) elves)
          ((eq? (car lst) #\.)
           (iter (cdr lst) (+ col-num 1) elves))
          (else
           (iter (cdr lst) (+ col-num 1)
                 (set-add elves (make-coord row-num col-num))))))
  (iter (string->list (string-trim row)) 0 elves))

(define (read-input file row-num elves)
  (let ((line (read-line file)))
    (if (eof-object? line) elves
        (read-input file (+ row-num 1) (read-row line row-num elves)))))

;;Coord get/set
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))

(define input-file (open-input-file "Test23.txt"))
(define input (read-input input-file 0 (set)))
(close-input-port input-file)
