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
  (let ((v (cdr node)))
    (cond ((> v 0) (+ v 1))
          (else v))))

;;Search lst for v, return its index
(define (search lst v)
  (define (iter lst v i)
    (if (equal? (car lst) v) i
        (iter (cdr lst) v (+ i 1))))
  (iter lst v 0))

(define (search-zero lst)
  (define (iter lst i)
    (if (= (cdr (car lst)) 0) i
        (iter (cdr lst) (+ i 1))))
  (iter lst 0))

(define (move item lst)
  (let ((split-point (modulo (+ (value item) (search lst item))
                             (length lst))))
    (append (remove item (take lst split-point))
            (list item)
            (remove item (drop lst split-point)))))

(define (move-all lst acc)
  (display (map cdr acc))
  (display "\r")
  (if (null? lst) acc
      (move-all (cdr lst) (move (car lst) acc))))

(define (calc-coords lst)
  (let ((zero (search-zero lst)))
    (+ (cdr (list-ref lst (modulo (+ zero 1000) (length lst))))
       (cdr (list-ref lst (modulo (+ zero 2000) (length lst))))
       (cdr (list-ref lst (modulo (+ zero 3000) (length lst)))))))

(define input (index-lst (read-input "Test20.txt")))
(define mixed-once (move-all input input))

(calc-coords mixed-once)
