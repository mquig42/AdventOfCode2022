;;;AoC_16.rkt
;;;2022-12-16
;;;Mike Quigley

;;;Optimize a sequence of actions, eh?

;;;I decided to read the input as an immutable hash table. Each value is a list
;;;containing the flow rate and all the other valves the exit tunnels lead to
;;;In my input, most of the valves have a flow rate of 0, so there's no point
;;;opening them. That should constrain the number of available moves.
#lang racket

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

(define input-file (open-input-file "Test16.txt"))
(define input (read-input input-file))
(close-input-port input-file)
