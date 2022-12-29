;;;AoC_16_Part2.rkt
;;;2022-12-28
;;;Mike Quigley

;;;I've already written a solution for day 16 part 1, but I don't think the
;;;approach I used would work for part 2. This is an attempt to solve both parts
;;;using the Racket Generic Graph Library

;;;Update: Reimplemented part 1. It works fast, with no memoization or
;;;heuristics. Clearly eliminating empty rooms helps a lot.
#lang racket
(require graph)

;;Same parser as before for the initial read from file
(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) (make-immutable-hash)
        (add-line (read-input file) line))))

(define (add-line input line)
  (let ((valves (map string->symbol (regexp-match* #px"\\p{Lu}{2}" line)))
        (flow-rate (string->number (car (regexp-match #px"\\d+" line)))))
    (hash-set input (car valves) (cons flow-rate (cdr valves)))))

;;Makes a list of all valves with non-zero flow rate
(define (list-non-zero valves)
  (filter (λ (x) (> (first (hash-ref valves x)) 0))
          (hash-keys valves)))

;;Generate a list of edges from one valve
(define (valve-edges valves valve)
  (define (iter exits)
    (if (null? exits) null
        (cons (list 1 valve (car exits))
              (iter (cdr exits)))))
  (iter (cdr (hash-ref valves valve))))

;;Generate a list of all edges
(define (valves-edges valves)
  (foldl append null
         (map (λ (x) (valve-edges valves x))
              (hash-keys valves))))

;;Determine maximum possible pressure released with given starting conditions
;;Position, remaining unopened valves, remaining minutes,
;;and total pressure released so far.
(define (max-flow pos v-rem m-rem acc)
  (let ((v-flow (car (hash-ref input pos))))
    (cond ((null? v-rem) (+ acc (* v-flow (max m-rem 0))))
          ((<= m-rem 1) acc)
          (else
           (argmax identity
                   (map (λ (valve)
                          (max-flow valve
                                    (remove valve v-rem)
                                    (- m-rem (hash-ref valves-distances
                                                       (list pos valve)) 1)
                                    (+ acc (* v-flow m-rem))))
                        v-rem))))))
  
(define input-file (open-input-file "Input16.txt"))
(define input (read-input input-file))
(close-input-port input-file)

;;Make a graph of the input, and generate a table of shortest distances
;;between each pair of rooms.
(define valves-graph (weighted-graph/directed (valves-edges input)))
(define valves-distances (floyd-warshall valves-graph))

(display "Part 1: ")
(max-flow 'AA (list-non-zero input) 30 0)

;;For part 2, assume we split the input in half
;;If the optimal solution actually involves me opening 9 valves and the elephant
;;opening 6, this approach won't find it. It does find optimal solutions to
;;both the example and my input.
(define nz-valves (list-non-zero input))
(define my-len (quotient (length nz-valves) 2))

(display "Part 2: ")
(argmax identity
        (map (λ (my-valves)
               (+ (max-flow 'AA my-valves 26 0)
                  (max-flow 'AA (remove* my-valves nz-valves) 26 0)))
             (combinations nz-valves my-len)))
