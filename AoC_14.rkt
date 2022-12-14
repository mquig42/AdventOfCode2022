;;;AoC_14.rkt
;;;2022-12-14
;;;Mike Quigley

;;;Now, this one could get interesting. And all I can see right now is part 1.
;;;I'll need to generate a map based on the input.
;;;Represent it as a set of points. Each point is a list of two numbers '(x y)
;;;I'll also need to enumerate a list of all the points between two endpoints.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-line (string-trim line)) (read-input file)))))

(define (parse-line line)
  (map (λ (x) (map string->number (string-split x ",")))
       (string-split line " -> ")))

;;Not sure this is needed, I just wanted to get an idea what size the map is
;;Gets the 4 points which have the minimum and maximum values for x and y
(define (get-extents lst)
  (list (argmin first (map (λ (x) (argmin first x)) lst))
        (argmax first (map (λ (x) (argmax first x)) lst))
        (argmin second (map (λ (x) (argmin second x)) lst))
        (argmax second (map (λ (x) (argmax second x)) lst))))

(define input-file (open-input-file "Test14.txt"))
(define input (read-input input-file))
(close-input-port input-file)
