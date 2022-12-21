#lang racket

;;How did I not know about file->lines? I've been implementing my own version
;;of it this whole time.
(define (read-input file)
  (map string->number (file->lines file)))

;;Adds a sequential index value to every item in lst
(define (index-lst lst)
  (map cons (range (length lst)) lst))

(define input (index-lst (read-input "Test20.txt")))
