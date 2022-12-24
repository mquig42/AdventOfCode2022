;;;AoC_23.rkt
;;;2022-12-23
;;;Mike Quigley

;;;Looks like it's Cellular Automaton time
;;;Starting this late because yesterday's cube of doom took so long

;;;Is it a real cellular automaton, though? It has entities that move,
;;;rather than cells that turn on or off depending on how many neighbours
;;;they have. Maybe it's possible to represent it using those sort of rules?

;;;Anyway, this is a working implementation but a bit slow.
;;;There's some redundancy. Proposed moves are generated twice.
;;;It takes about 2 seconds to solve part 1, and 90 seconds for part 2.
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
(define (add-coords a b)
  (make-coord (+ (get-row a) (get-row b)) (+ (get-col a) (get-col b))))

;;Move one element from the head of a list to the tail
(define (rotate lst)
  (append (drop lst 1) (take lst 1)))

;;Get the 3 neighnouring coords in given direction
;;For example, if direction is North, return N, NW, and NE neighbours
(define (get-neighbours coord direction)
  (cond ((equal? direction N)
         (set (add-coords coord N)
              (add-coords coord NE)
              (add-coords coord NW)))
        ((equal? direction S)
         (set (add-coords coord S)
              (add-coords coord SE)
              (add-coords coord SW)))
        ((equal? direction W)
         (set (add-coords coord W)
              (add-coords coord NW)
              (add-coords coord SW)))
        ((equal? direction E)
         (set (add-coords coord E)
              (add-coords coord NE)
              (add-coords coord SE)))))

;;Returns true if an elf has no neighbours in any direction
(define (no-neighbours? elf elves)
  (= 0 (set-count
        (set-intersect
         (set-union (get-neighbours elf N)
                    (get-neighbours elf E)
                    (get-neighbours elf S)
                    (get-neighbours elf W))
         elves))))

;;Returns the proposed move for a single elf
(define (propose-move all-elves elf dirs)
  (cond ((null? dirs) elf)
        ((no-neighbours? elf all-elves) elf)
        ((= (set-count (set-intersect all-elves
                                      (get-neighbours elf
                                                      (car dirs))))
            0)
         (add-coords elf (car dirs)))
        (else (propose-move all-elves elf (cdr dirs)))))

;;Generate a hash table of proposed moves
(define (propose-moves all-elves dirs)
  (define (iter elves moves)
    (if (null? elves) moves
        (let ((move (propose-move all-elves (car elves) dirs)))
          (iter (cdr elves)
                (hash-set moves
                          move
                          (+ 1 (hash-ref moves move 0)))))))
  (iter (set->list all-elves) (make-immutable-hash)))

;;Returns a set of elves after each has made one move
(define (make-moves proposed-moves all-elves dirs)
  (define (iter elves acc)
    (if (null? elves) acc
        (let* ((p-move (propose-move all-elves (car elves) dirs))
               (move (if (= 1 (hash-ref proposed-moves p-move))
                         p-move
                         (car elves))))
          (iter (cdr elves)
                (set-add acc move)))))
  (iter (set->list all-elves) (set)))

;;Returns set of elf coordinates after n rounds
(define (make-n-moves n elves dirs)
  (if (= n 0) elves
      (make-n-moves (- n 1)
                    (make-moves (propose-moves elves dirs)
                                elves dirs)
                    (rotate dirs))))

;;Returns number of rounds needed to reach a static state
(define (count-moves n elves dirs)
  (let ((elves-next (make-moves (propose-moves elves dirs)
                                elves dirs)))
    (if (set=? elves elves-next) n
        (count-moves (+ n 1) elves-next (rotate dirs)))))

;;Gets the minimum and maximum row and column values of the set
(define (get-extents elves)
  (define (iter elves min-row max-row min-col max-col)
    (if (null? elves) (list (inexact->exact min-row)
                            (inexact->exact max-row)
                            (inexact->exact min-col)
                            (inexact->exact max-col))
        (iter (cdr elves)
              (min min-row (get-row (car elves)))
              (max max-row (get-row (car elves)))
              (min min-col (get-col (car elves)))
              (max max-col (get-col (car elves))))))
  (iter (set->list elves) +inf.0 -inf.0 +inf.0 -inf.0))

;;Area of a rectangle defined by a list of extents
(define (area extents)
  (* (+ 1 (- (second extents) (first extents)))
     (+ 1 (- (fourth extents) (third extents)))))

(define input-file (open-input-file "Input23.txt"))
(define input (read-input input-file 0 (set)))
(close-input-port input-file)

;;Directions
(define N (make-coord -1 0))
(define NE (make-coord -1 1))
(define E (make-coord 0 1))
(define SE (make-coord 1 1))
(define S (make-coord 1 0))
(define SW (make-coord 1 -1))
(define W (make-coord 0 -1))
(define NW (make-coord -1 -1))
(define directions (list N S W E))

(display "Part 1: ")
(- (area (get-extents (make-n-moves 10 input directions)))
   (set-count input))

(display "Part 2: ")
(count-moves 1 input directions)
