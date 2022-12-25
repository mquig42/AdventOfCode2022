;;;AoC_24.rkt
;;;2022-12-24
;;;Mike Quigley

;;;So, this is a pathfinding problem through a valley of constantly
;;;shifting storms. Not sure if standard pathfinding algos will work here,
;;;so let's start with a bread-first search. That means I'll need a queue again.

;;;Update: tried to use a linked list as a queue. This produces the correct
;;;answer on the sample input, but is slow.
;;;Update: It's not just slow, it runs out of memory. I need a smaller way
;;;of representing state instead of storing the position of every storm
#lang racket

;;Read one row of input
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

;;Read all input
(define (read-input file row-no walls n-storms e-storms s-storms w-storms)
  (let ((line (read-line file)))
    (if (eof-object? line) (list walls n-storms e-storms s-storms w-storms)
        (let ((row-values
               (read-row line row-no
                          walls n-storms e-storms s-storms w-storms)))
          (read-input file (+ row-no 1)
                      (first row-values) (second row-values) (third row-values)
                      (fourth row-values) (fifth row-values))))))

;;Getters
(define get-row car)
(define get-col cdr)
(define (add-coords a b)
  (cons (+ (get-row a) (get-row b))
        (+ (get-col a) (get-col b))))

;;Directions
(define N '(-1 . 0))
(define E '(0 . 1))
(define S '(1 . 0))
(define W '(0 . -1))

;;Functions for storm movement
(define (move-storm-n pos wrap-from wrap-to)
  (let ((new-pos (add-coords pos N)))
    (if (= (get-row new-pos) wrap-from) (cons wrap-to (get-col new-pos))
        new-pos)))
(define (move-storm-e pos wrap-from wrap-to)
  (let ((new-pos (add-coords pos E)))
    (if (= (get-col new-pos) wrap-from) (cons (get-row new-pos) wrap-to)
        new-pos)))
(define (move-storm-s pos wrap-from wrap-to)
  (let ((new-pos (add-coords pos S)))
    (if (= (get-row new-pos) wrap-from) (cons wrap-to (get-col new-pos))
        new-pos)))
(define (move-storm-w pos wrap-from wrap-to)
  (let ((new-pos (add-coords pos W)))
    (if (= (get-col new-pos) wrap-from) (cons (get-row new-pos) wrap-to)
        new-pos)))

;;Moves all storms. Input and output are both list containing
;;North, East, South, and West storms in that order
(define (move-all-storms storms)
  (list (list->set (set-map (first storms)
                            (λ (x) (move-storm-n x 0 (- max-row 1)))))
        (list->set (set-map (second storms)
                            (λ (x) (move-storm-e x max-col 1))))
        (list->set (set-map (third storms)
                            (λ (x) (move-storm-s x max-row 1))))
        (list->set (set-map (fourth storms)
                            (λ (x) (move-storm-w x 0 (- max-col 1)))))))

;;Enumerate all possible moves from coord.
(define (enumerate-moves coord)
  (set coord
       (add-coords coord N)
       (add-coords coord E)
       (add-coords coord S)
       (add-coords coord W)))

(define (make-state pos dist storms-lst)
  (cons pos (cons dist storms-lst)))

;;Bread first search. Uses a list as a queue, which may be slow.
(define (route-search states)
  (let* ((current-state (car states))
         (pos (first current-state))
         (dist (second current-state))
         (storms-next (cons (third current-state)
                            (move-all-storms (drop current-state 3))))
         (moves (set->list (set-subtract (enumerate-moves pos)
                                         (first storms-next)
                                         (second storms-next)
                                         (third storms-next)
                                         (fourth storms-next)
                                         (fifth storms-next)))))
    (if (= max-row (get-row pos)) dist
        (route-search
         (append (cdr states)
                 (map (λ (x) (make-state x (+ dist 1) storms-next))
                      moves))))))


(define input-file (open-input-file "Input24.txt"))
;Walls starts with one value, '(-1 . 1), to block the entrance
(define input (read-input input-file 0 (set '(-1 . 1)) (set) (set) (set) (set)))
(close-input-port input-file)

;Find maximum row and column values for storm wrapping and exit identification.
(define max-col (argmax identity (map get-col (set->list (first input)))))
(define max-row (argmax identity (map get-row (set->list (first input)))))

(display "Part 1: ")
(time (route-search (list (make-state '(0 . 1) 0 input))))
