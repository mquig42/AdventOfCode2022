;;;AoC_12.rkt
;;;2022-12-12
;;;Mike Quigley

;;;Need to find a shortest path. Time for Dijkstra's algorithm again.
#lang racket
(require data/heap)

;;I'm storing the input as a vector, not because it's mutable, but for speed
(define (read-input file)
  (define (iter)
    (let ((line (read-line file)))
      (if (eof-object? line) null
          (cons (list->vector (string->list (string-trim line))) (iter)))))
  (list->vector (iter)))

;;Getters and setters for coord
(define (make-coord row col)
  (cons row col))
(define (get-row coord)
  (car coord))
(define (get-col coord)
  (cdr coord))

;;Queue functions
;Creates a queue element, which has a priority and value
(define (q-element priority value)
  (cons priority value))
;Compare priority of two queue elements
(define (q<= a b)
  (<= (car a) (car b)))
;Creates a new queue with the given values
(define (q-init values)
  (let ((q (make-heap q<=)))
    (for-each (λ (x) (heap-add! q (q-element +inf.0 x))) values)
    q))
;Changes an element's priority
(define (q-reduce-priority! q value from to)
  (heap-remove-eq! q (q-element from value))
  (heap-add! q (q-element to value)))
;Returns value of element with lowest priority and removes it from queue
(define (q-lowest! q)
  (let ((lowest (heap-min q)))
    (heap-remove-min! q)
    lowest))

;;getters for input
(define (get-height grid coord)
  (if (or (< (get-row coord) 0)
          (>= (get-row coord) (vector-length grid))
          (< (get-col coord) 0)
          (>= (get-col coord) (vector-length (vector-ref grid 0))))
      #f
      (vector-ref (vector-ref grid (get-row coord)) (get-col coord))))
(define (end? grid coord)
  (eq? (get-height grid coord) #\E))
(define (start? grid coord)
  (eq? (get-height grid coord) #\S))

;;Returns a list of every coord in grid
(define (enumerate-coords grid)
  (define (iter row col)
    (cond ((>= row (vector-length grid)) null)
          ((>= col (vector-length (vector-ref grid 0)))
           (iter (+ row 1) 0))
          (else (cons (make-coord row col) (iter row (+ col 1))))))
  (iter 0 0))

;;Is a move between the two coords valid?
;;Note: only compares height, not distance
(define (valid-move? grid from to)
  (or (start? grid from) ;Any move from starting point is valid
      (and (end? grid to) (eq? (get-height grid from) #\z)) ;end height is z
      (and (not (end? grid to))
           (<= (char->integer (get-height grid to))
               (+ 1 (char->integer (get-height grid from)))))))

;;Gets a list of all coords that can be moved to
(define (get-valid-moves grid from)
  (filter (λ (to) (valid-move? grid from to))
          (filter (λ (coord) (get-height grid coord))
                  (list (make-coord (- (get-row from) 1) (get-col from))
                        (make-coord (+ (get-row from) 1) (get-col from))
                        (make-coord (get-row from) (- (get-col from) 1))
                        (make-coord (get-row from) (+ (get-col from) 1))))))

;;Starting point isn't always in the same place, need to search for it
;;Note: will infinite loop if grid doesn't contain a valid atarting point
(define (find-start grid)
  (car (filter (λ (coord) (start? grid coord)) (enumerate-coords grid))))

;;Returns length of shortest path from start to end in grid
(define (shortest-path grid start-coord)
  (let ((unvisited (q-init (enumerate-coords grid)))
        (visited (mutable-set))
        (distances (make-hash)))
    (define (visit node) ;Node is a q-element
      (let ((dist (car node))
            (coord (cdr node)))
        (cond ((end? grid coord) dist)
              (else (for-each (λ (x)
                                (let ((old-dist (hash-ref distances x +inf.0))
                                      (new-dist (+ dist 1)))
                                  (cond ((> old-dist new-dist)
                                         (hash-set! distances x new-dist)
                                         (q-reduce-priority!
                                          unvisited x old-dist new-dist)))))
                              (filter (λ (x) (not (set-member? visited x)))
                                      (get-valid-moves grid coord)))
                    (set-add! visited coord)
                    (visit (q-lowest! unvisited))))))
    (q-reduce-priority! unvisited start-coord +inf.0 0)
    (hash-set! distances start-coord 0)
    (visit (q-lowest! unvisited))))
                      
    

(define input-file (open-input-file "Input12.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(shortest-path input (find-start input))

;This takes a couple of minutes to run. An idea to speed it up:
;Reverse the valid-move? function so it can go up any level but only down 1
;Part 1 searches for shortest path from E to S
;Part 2 searches for shortest path from E to a
(display "Part 2: \r")
(time (shortest-path input
                     (argmin (λ (x) (shortest-path input x))
                             (filter (λ (x) (eq? (get-height input x) #\a))
                                     (enumerate-coords input)))))
