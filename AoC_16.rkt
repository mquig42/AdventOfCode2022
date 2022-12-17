;;;AoC_16.rkt
;;;2022-12-16
;;;Mike Quigley

;;;Optimize a sequence of actions, eh?
;;;At first, I thought of just looking at the flow rates of all the valves
;;;I can reach from my current location and moving to the biggest one, but
;;;the description has an optimal sequence for the sample data that's different.
;;;This seems similar to day 12 last year. I solved that by enumerating every
;;;possible path, so I could try that here.

;;;I decided to read the input as an immutable hash table. Each value is a list
;;;containing the flow rate and all the other valves the exit tunnels lead to
;;;In my input, most of the valves have a flow rate of 0, so there's no point
;;;opening them. That should constrain the number of available moves.

;;;Here's an idea: generate shorter sequences and prune them
;;;There are 42,450 possible length-10 sequences, but the optimal answer
;;;will probably be one of the top thousand (maybe even the top hundred).
;;;Also, the total sequence length may be 31, since the sequences generated
;;;by this program include the starting point. Not sure.
#lang racket
(require memo)

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) (make-immutable-hash)
        (add-line (read-input file) line))))

;;Adds the data from line to the input hashtable
(define (add-line input line)
  (let ((valves (regexp-match* #px"\\p{Lu}{2}" line)) ;2 uppercase letters
        (flow-rate (string->number (car (regexp-match #px"\\d+" line)))))
    (hash-set input (car valves) (cons flow-rate (cdr valves)))))

;;Getters
(define (flow-rate valves valve)
  (car (hash-ref valves valve)))
(define (exits valves valve)
  (cdr (hash-ref valves valve)))

;;Returns list of all possible moves from current position
;;This includes all exits, and the flow rate if it isn't 0
(define (moves valves valve)
  (filter (λ (x) (not (eq? x 0))) (hash-ref valves valve)))

;;Returns a modified hash table with the given valve set to 0
;;to simulate it being turned
(define (turn-valve valves valve)
  (hash-set valves valve
            (list-set (hash-ref valves valve) 0 0)))

(define (enumerate-all-paths start path valves v-rem m-rem)
  (let ((path (cons start path))
        (valves (if (number? start) (turn-valve valves (car path)) valves))
        (v-rem (if (number? start) (- v-rem 1) v-rem))
        (m-rem (- m-rem 1))
        (start (if (number? start) (car path) start)))
    (if (or (= v-rem 0) (= m-rem 0)) (list (reverse path))
        (foldl (λ (x acc)
                 (append acc (enumerate-all-paths x path valves v-rem m-rem)))
               null
               (moves valves start)))))

;;Calculates the total pressure released by a given sequence of moves
;;Sequence can be less than 30 moves, if it's possible to open every valve
;;faster than that (it is for the test input)
(define (score seq)
  (define (iter seq acc mins)
    (cond ((null? seq) acc)
          ((number? (car seq))
           (iter (cdr seq) (+ acc (* mins (car seq))) (- mins 1)))
          (else
           (iter (cdr seq) acc (- mins 1)))))
  (iter seq 0 30))

;;Returns number of valves with a non-zero flow rate
(define (count-non-zero valves)
  (length (filter (λ (x) (not (= 0 (car x)))) (hash-values valves))))

;;Finds optimal sequence of given length
(define (find-optimal-seq valves m-rem)
  (argmax score
          (enumerate-all-paths "AA" null valves (count-non-zero valves) m-rem)))

;;Let's try dynamic programming. Return maximum possible score with given
;;starting conditions
(define/memoize (max-flow loc valves m-rem acc) #:hash hash
  (cond ((<= m-rem 0) acc)
        ((= (count-non-zero valves) 0) acc)
        (else
         (let ((max-no-turn
                (argmax identity
                        (map (λ (x) (max-flow x valves (- m-rem 1) acc))
                             (exits valves loc))))
               (max-turn
                (if (= (flow-rate valves loc) 0) 0
                       (argmax
                        identity
                        (map
                         (λ (x) (max-flow
                                 x
                                 (turn-valve valves loc)
                                 (- m-rem 2)
                                 (+ acc
                                    (* (flow-rate valves loc) (- m-rem 1)))))
                         (exits valves loc))))))
           (max max-turn max-no-turn)))))
        

(define input-file (open-input-file "Input16.txt"))
(define input (read-input input-file))
(close-input-port input-file)

;optimal sequence for sample input
(define optimal-sample '("AA" "DD" 20 "CC" "BB" 13 "AA" "II" "JJ" 21 "II" "AA" "DD" "EE" "FF" "GG" "HH" 22 "GG" "FF" "EE" 3 "DD" "CC" 2))
