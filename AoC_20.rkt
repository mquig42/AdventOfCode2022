;;;AoC_20.rkt
;;;2022-12-20
;;;Mike Quigley

;;;Getting the movement logic to work here took me too long to get right.
;;;Off-by-one errors in the list indexing.
;;;Once I solved part 1, part 2 took less than 10 minutes.
#lang racket

;;How did I not know about file->lines? I've been implementing my own version
;;of it this whole time.
(define (read-input file)
  (map string->number (file->lines file)))

;;Adds a sequential index value to every item in lst
(define (index-lst lst)
  (map cons (range (length lst)) lst))

;;Getters
(define (value node)
  (cdr node))

;;Applies decryption key to a node
(define (decrypt node)
  (cons (car node) (* (cdr node) 811589153)))

;;Search lst for v, return its index
(define (search lst v)
  (define (iter lst v i)
    (if (equal? (car lst) v) i
        (iter (cdr lst) v (+ i 1))))
  (iter lst v 0))

;;Find the index of the node with 0 value
(define (search-zero lst)
  (define (iter lst i)
    (if (= (cdr (car lst)) 0) i
        (iter (cdr lst) (+ i 1))))
  (iter lst 0))

;;Moves a single item in the list
(define (move item lst)
  (let* ((idx (+ (search lst item) (value item)))
         (tmp-lst (remove item lst))
         (split-point (modulo idx (length tmp-lst))))
    (append (take tmp-lst split-point)
            (list item)
            (drop tmp-lst split-point))))

;;moves all items in the list once
;;Takes the same list as input twice. The first is used for instructions,
;;the second for intermediate storage
(define (move-all lst acc)
  (if (null? lst) acc
      (move-all (cdr lst) (move (car lst) acc))))

(define (calc-coords lst)
  (let ((zero (search-zero lst)))
    (+ (cdr (list-ref lst (modulo (+ zero 1000) (length lst))))
       (cdr (list-ref lst (modulo (+ zero 2000) (length lst))))
       (cdr (list-ref lst (modulo (+ zero 3000) (length lst)))))))

(define input (index-lst (read-input "Input20.txt")))

(display "Part 1: ")
(define mixed-once (move-all input input))
(calc-coords mixed-once)

(display "Part 2: ")
(define decrypted-input (map decrypt input))
;Too lazy to use a counter. Just repeat the process by repeating the list.
(define mixed-ten (move-all (append decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input
                                    decrypted-input)
                            decrypted-input))
(calc-coords mixed-ten)
