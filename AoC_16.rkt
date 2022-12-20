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

;;;I just spent quite a lot of time on dynamic programming, which worked on
;;;the sample but is too slow for the full input. Maybe that pruning idea
;;;will work better.

;;;Another update: I tried adding a simple enhancement to my DP solution.
;;;Assume that large valves are opened early. If, after 10 minutes, I haven't
;;;opened enough valves to release 1000 units of pressure over the remaining
;;;time, then assume the solution is suboptimal and return 0
;;;This solves part 1 in under 2 seconds.
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

;;Returns a modified hash table with the given valve set to 0
;;to simulate it being turned
(define (turn-valve valves valve)
  (hash-set valves valve
            (list-set (hash-ref valves valve) 0 0)))

;;Returns number of valves with a non-zero flow rate
(define (count-non-zero valves)
  (length (filter (λ (x) (not (= 0 (car x)))) (hash-values valves))))

;;Let's try dynamic programming. Return maximum possible score with given
;;starting conditions
(define/memoize (max-flow state) #:hash hash
  (let ((loc (first state))
        (valves (second state))
        (m-rem (third state))
        (v-rem (fourth state))
        (acc (fifth state)))
  (cond ((= m-rem 0) acc)
        ((= v-rem 0) acc)
        ((and (< m-rem 20) (< acc 1000)) 0)
        (else
         (let ((max-no-turn
                (argmax identity
                        (map (λ (x)
                               (max-flow (list x valves (- m-rem 1) v-rem acc)))
                             (exits valves loc))))
               (max-turn
                (if (= (flow-rate valves loc) 0) 0
                       (max-flow (list loc (turn-valve valves loc)
                                 (- m-rem 1)
                                 (- v-rem 1)
                                 (+ (* (flow-rate valves loc) (- m-rem 1))
                                    acc))))))
           (max max-turn max-no-turn))))))
        

(define input-file (open-input-file "Input16.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(max-flow (list "AA" input 30 (count-non-zero input) 0))
