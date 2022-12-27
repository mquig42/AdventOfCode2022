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
;;;Update: improved everything. States are small, and I used a proper queue
;;;instead of a list. It still doesn't reach an answer after running all night.
;;;Time for heuristics. Maybe a "no backtracking" rule. Don't move to the same
;;;space you just moved from. This also means no waiting for two minutes in
;;;a row. I'm not sure if this would get the correct answer, but it would cut
;;;the search space.
;;;Update: Implemented a no backtracking rule. It's now very fast, but doesn't
;;;reach a solution. I guess backtracking is neccessary.
;;;Storms have a period of 600 minutes. Can use that to prune the search.
;;;Actually, that periodicity doesn't matter. My answer for part 1 is 308.
;;;which is less than the period. Paths can still branch and merge, so
;;;tracking previously visited states still reduces runtime a lot. Solving
;;;part 1 takes ~260 seconds.

;;;Performance still isn't great. Maybe try a priority queue based on manhattan
;;;distance to endpoint.
#lang racket
(require queue)

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

;;Move storm horizontal or vertical. Can do any number of timesteps.
(define (move-storm-h pos dir steps width)
  (cons (get-row pos)
        (+ 1 (modulo (+ (* steps (get-col dir)) (- (get-col pos) 1)) width))))
(define (move-storm-v pos dir steps height)
  (cons (+ 1 (modulo (+ (* steps (get-row dir)) (- (get-row pos) 1)) height))
        (get-col pos)))

;;Finds the position of all storms at given time
(define (move-all-storms storms steps)
  (list (list->set (set-map (first storms)
                            (λ (x) (move-storm-v x N steps (- max-row 1)))))
        (list->set (set-map (second storms)
                            (λ (x) (move-storm-h x E steps (- max-col 1)))))
        (list->set (set-map (third storms)
                            (λ (x) (move-storm-v x S steps (- max-row 1)))))
        (list->set (set-map (fourth storms)
                            (λ (x) (move-storm-h x W steps (- max-col 1)))))))

;;Storms might have a period. Find it
(define (find-period storms steps)
  (if (equal? (move-all-storms storms steps) storms) steps
      (find-period storms (+ steps 1))))

;;Enumerate all possible moves from coord.
(define (enumerate-moves coord)
  (set coord
       (add-coords coord N)
       (add-coords coord E)
       (add-coords coord S)
       (add-coords coord W)))

;;Bread first search. Uses a list as a queue, which may be slow.
(define (route-search states initial-state dest-row visited)
  (define-values (current-state q) (queue-remove states))
  (let* ((pos (first current-state))
         (dist (second current-state))
         (storms-next (cons (first initial-state)
                            (move-all-storms (drop initial-state 1)
                                             (+ dist 1))))
         (moves (set->list (set-subtract (enumerate-moves pos)
                                         (first storms-next)
                                         (second storms-next)
                                         (third storms-next)
                                         (fourth storms-next)
                                         (fifth storms-next)))))
    ;(printf "~a\r" dist)
    (cond  ((= dest-row (get-row pos)) current-state)
           ((set-member? visited current-state)
            (route-search q initial-state dest-row visited))
           (else
            (route-search (queue-add-list
                           q
                           (map (λ (x) (list x (+ dist 1))) moves))
                          initial-state
                          dest-row
                          (set-add visited current-state))))))

;;Adds all items from a list into a queue, in order
(define  (queue-add-list q lst)
  (if (null? lst) q
      (queue-add-list (queue-add q (car lst)) (cdr lst))))

(define input-file (open-input-file "Input24.txt"))
;Walls starts with one value, '(-1 . 1), to block the entrance
(define input (read-input input-file 0 (set '(-1 . 1)) (set) (set) (set) (set)))
(close-input-port input-file)

;Find maximum row and column values for storm wrapping and exit identification.
(define max-col (argmax identity (map get-col (set->list (first input)))))
(define max-row (argmax identity (map get-row (set->list (first input)))))

(time
 (define first-trip (route-search (queue-add (make-queue) (list '(0 . 1) 0))
                                  input
                                  max-row
                                  (set)))
 (printf "First crossing at t=~a\r" (second first-trip))

 (define input2
   (list-set input 0
             (set-add (first input) (add-coords (first first-trip) S))))
 (define return-trip (route-search (queue-add (make-queue) first-trip)
                                   input2
                                   0
                                   (set)))
 (printf "Returned to start at t=~a\r" (second return-trip))

 (define second-trip (route-search (queue-add (make-queue) return-trip)
                                   input2
                                   max-row
                                   (set)))
 (printf "Second crossing at t=~a\r" (second second-trip)))
