;;;AoC_17.rkt
;;;2022-12-17
;;;Mike Quigley

;;;Decided to represent the board with lists of 1s and 0s. Each level is a list
;;;of 7 values, and the overall structure is a list of levels. I add new levels
;;;to the head.

;;;The real challenge is part 2. I wrote a simple hash function, ran it on the
;;;entire board after dropping 10,000 rocks, and dumped the results into Gnuplot
;;;The board has a repeating pattern of length 2642, which is a lot shorter
;;;than I was expecting. I should be able to exploit that to skip a lot
;;;of steps. This will involve some manual analysis.

;;;Exact details of the pattern: The hash value 88, representing a level state
;;;of (1 0 1 1 0 0 0), occurs twice in a row, once per cycle. This value is
;;;important only in its rarity, but I can regard it as the cycle's endpoint.

;;;If I drop 200 rocks, the hash values at the top of the stack are
;;;24, 24, 16, 18, 114, 18, 58, 31, 14...
;;;The 24s are the levels that will become 88 when 204 rocks are dropped.
;;;The same pattern occurs at the top of the stack after dropping 1900 rocks
;;;and 3600, and 5300 and so on. The cycle length is 1700 rocks

;;;Oddly enough, the goal of 1000000000000 rocks is (1700 * 588235294) + 200
;;;Having 200 and 1700 in there looks like I was meant to find this exact
;;;pattern. Maybe it's set up so hash value 88 occurs exactly where it does
;;;on purpose.
;;;Let's try using math to predict sim results.
;;;Height after n rocks:
;;;   200: 322
;;;  1900: 2964
;;;  3600: 5606
;;;That's a difference of 2642 each time, as expected.
;;;Let's try dropping (1700 * 12) + 200 = 20600 rocks.
;;;Height should be (2642 * 12) + 322 = 32026
;;;Tried running the sim, and got that exact answer.
;;;In that case, part 2 is (2642 * 588235294) + 322 = 1554117647070
;;;And, confirmed! That's my final star for 2022
#lang racket

(define (read-input file)
  (map (λ (x) (if (eq? x #\<) '(0 . -1) '(0 . 1)))
       (string->list (string-trim (read-line file)))))

;;Coord getters
(define get-row car)
(define get-col cdr)
(define make-coord cons)
(define (add-coords a b)
  (make-coord (+ (get-row a) (get-row b))
              (+ (get-col a) (get-col b))))

;;Lookup position on board
(define (board-ref b coord)
  (list-ref (list-ref b (get-row coord)) (get-col coord)))

;;Sets given coord to 1
(define (board-set b coord)
  (list-set b (get-row coord)
            (list-set (list-ref b (get-row coord)) (get-col coord) 1)))

;;I have a feeling I'll be using this a lot
(define (inc-mod x m)
  (modulo (+ x 1) m))

;;Trims empty levels off the top of the board
(define (board-trim b)
  (if (equal? (car b) '(0 0 0 0 0 0 0))
      (board-trim (cdr b))
      b))

;;Adds n empty levels to the top of board
(define (add-levels b n)
  (if (= n 0) b
      (add-levels (cons '(0 0 0 0 0 0 0) b) (- n 1))))

;;Adds a rock to the board at given coords
(define (add-rock rock coord b)
  (if (null? rock) b
      (add-rock (cdr rock) coord (board-set b (add-coords (car rock) coord)))))

;;Returns true if rock can move into given coords, false if it can't
(define (can-move? rock coord b)
  (cond ((null? rock) true)
        ((< (+ (get-col (car rock)) (get-col coord)) 0) false)
        ((> (+ (get-col (car rock)) (get-col coord)) 6) false)
        ((= (board-ref b (add-coords (car rock) coord)) 1) false)
        (else (can-move? (cdr rock) coord b))))

;;Each iteration of this function runs one sim cycle: that is,
;;one move across and one move down. If it's not possible to move down,
;;it enlarges the board and moves on to the next rock.
;;To make room for a new rock, add 7 levels and start it at (3 . 2)
(define (run-sim b rock-id vent-id coord rock-counter goal)
  (if (= rock-counter goal) b
      (let* ((v-coord (add-coords coord (vector-ref input vent-id)))
             (new-col (if (can-move? (rocks rock-id) v-coord b)
                          v-coord
                          coord))
             (new-row (add-coords new-col '(1 . 0)))
             (new-vent (inc-mod vent-id (vector-length input))))
        (if (can-move? (rocks rock-id) new-row b)
            (run-sim b rock-id new-vent new-row rock-counter goal)
            (run-sim
             (add-levels
              (board-trim
               (add-rock (rocks rock-id) new-col b)) 7)
             (inc-mod rock-id 5)
             new-vent
             '(3 . 2)
             (+ rock-counter 1)
             goal)))))

;;Generates a unique integer representation of a filled level
;;Since a level is a list of 1s and 0s, just convert it from binary to decimal.
(define (hash-level level)
  (define (iter lst p2 acc)
    (if (null? lst) acc
        (iter (cdr lst) (/ p2 2) (+ acc (* (car lst) p2)))))
  (iter level 64 0))
                         

(define input-file (open-input-file "Input17.txt"))
(define input (list->vector (read-input input-file)))
(close-input-port input-file)

;Starting condition
(define board (list (list 1 1 1 1 1 1 1)))

;Rocks
(define r1 '((0 . 0) (0 . 1) (0 . 2) (0 . 3)))
(define r2 '((0 . 1) (-1 . 0) (-1 . 1) (-1 . 2) (-2 . 1)))
(define r3 '((0 . 0) (0 . 1) (0 . 2) (-1 . 2) (-2 . 2)))
(define r4 '((0 . 0) (-1 . 0) (-2 . 0) (-3 . 0)))
(define r5 '((0 . 0) (0 . 1) (-1 . 0) (-1 . 1)))
(define (rocks r)
  (cond ((= r 0) r1)
        ((= r 1) r2)
        ((= r 2) r3)
        ((= r 3) r4)
        ((= r 4) r5)))

(display "Part 1: ")
(- (length (board-trim (run-sim (add-levels board 7) 0 0 '(3 . 2) 0 2022))) 1)

(display "Part 2: See comment block at beginning of file for math")
