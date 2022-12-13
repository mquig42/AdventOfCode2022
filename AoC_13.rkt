;;;AoC_13.rkt
;;;2022-12-13
;;;Mike Quigley

;;;Today is all about lists. Reading input was straightforward, just get rid of
;;;the commas and let the LISP parser do the rest of the work.
#lang racket

(define (read-input file)
  (let ((line1 (read-line file))
        (line2 (read-line file))
        (line3 (read-line file)))
    (if (eof-object? line1) null
        (cons (list (parse-line line1) (parse-line line2))
              (read-input file)))))

;;Convert a single line of input into a list of (lists of) integers
(define (parse-line str)
  (read (open-input-string (string-replace (string-trim str) "," " "))))

(define input-file (open-input-file "Test13.txt"))
(define input (read-input input-file))
(close-input-port input-file)
