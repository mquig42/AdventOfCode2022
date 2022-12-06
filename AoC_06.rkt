;;;AoC_06.rkt
;;;2022-12-06
;;;Mike Quigley

;;;Solved this one quickly using hash sets. I've left my initial
;;;part 1 solution, though find-n-different can solve either part.
#lang racket
(require racket/set)

;;Takes 4 arguments and returns true if any of them are equal,
;;or false if they are all different
(define (equal4? p1 p2 p3 p4)
  (or (equal? p1 p2)
      (equal? p1 p3)
      (equal? p1 p4)
      (equal? p2 p3)
      (equal? p2 p4)
      (equal? p3 p4)))

;;Using a sliding window to find the first instance of 4 distinct items in lst
(define (find-buffer lst)
  (define (iter p1 p2 p3 lst i)
    (if (equal4? p1 p2 p3 (car lst)) (iter p2 p3 (car lst) (cdr lst) (+ i 1))
        i))
  (iter (first lst) (second lst) (third lst) (drop lst 3) 4))

;;Finds the first instance of n distinct items in lst
;;Hash sets make this easy, though it is O(m*n), and O(m) is probably possible
;;(for m as length of input and n as length of the substring, ie 14 for part 2)
(define (find-n-different lst n)
  (define (iter lst i)
    (if (= (set-count (list->set (take lst n))) n) i
        (iter (cdr lst) (+ i 1))))
  (iter lst n))

(define input-file (open-input-file "Input06.txt"))
(define input (string->list (string-trim (read-line input-file))))
(close-input-port input-file)

(display "Part 1: ")
(find-buffer input)
(display "Part 2: ")
(find-n-different input 14)
