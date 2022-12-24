;;;AoC_24.rkt
;;;2022-12-24
;;;Mike Quigley

;;;So, this is a pathfinding problem through a valley of constantly
;;;shifting storms. Not sure if standard pathfinding algos will work here,
;;;so let's start with a bread-first search. That means I'll need a queue again.
#lang racket

(define (read-row line row-no walls n-storms e-storms s-storms w-storms)
  (define (iter lst col-no walls n-storms e-storms s-storms w-storms)
    (cond ((null? lst) (list walls n-storms e-storms s-storms w-storms))
          ((eq? (car lst) #\#)
           (iter (cdr lst) (+ col-no 1)
                 (set-add walls (cons row-no col-no))
                 n-storms e-storms s-storms w-storms))
          ((eq? (car lst) #\^)
           (iter (cdr lst) (+ col-no 1) walls
                 (set-add n-storms (cons row-no col-no))
                 e-storms s-storms w-storms))
          ((eq? (car lst) #\>)
           (iter (cdr lst) (+ col-no 1) walls n-storms
                 (set-add e-storms (cons row-no col-no))
                 s-storms w-storms))
          ((eq? (car lst) #\v)
           (iter (cdr lst) (+ col-no 1) walls n-storms e-storms
                 (set-add s-storms (cons row-no col-no))
                 w-storms))
          ((eq? (car lst) #\<)
           (iter (cdr lst) (+ col-no 1) walls n-storms e-storms s-storms
                 (set-add w-storms (cons row-no col-no))))
          (else (iter (cdr lst) (+ col-no 1)
                      walls n-storms e-storms s-storms w-storms))))
  (iter (string->list (string-trim line)) 0
        walls n-storms e-storms s-storms w-storms))

(define (read-input file row-no walls n-storms e-storms s-storms w-storms)
  (let ((line (read-line file)))
    (if (eof-object? line) (list walls n-storms e-storms s-storms w-storms)
        (let ((row-values
               (read-row line row-no
                          walls n-storms e-storms s-storms w-storms)))
          (read-input file (+ row-no 1)
                      (first row-values) (second row-values) (third row-values)
                      (fourth row-values) (fifth row-values))))))

(define input-file (open-input-file "Test24.txt"))
(define input (read-input input-file 0 (set) (set) (set) (set) (set)))
(close-input-port input-file)
