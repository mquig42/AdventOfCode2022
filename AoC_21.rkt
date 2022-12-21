;;;AoC_21.rkt
;;;2022-12-21
;;;Mike Quigley

;;;Instead of solving the problem,
;;;I can reformat the input into a program and then run it
#lang racket

(define input (file->lines "Input21.txt"))

(define (format-line line)
  (let ((tokens (string-split (string-replace line ":" ""))))
    (if (= 2 (length tokens))
        (format "(define (~a) ~a)" (first tokens) (second tokens))
        (format "(define (~a) (~a (~a) (~a)))"
                (first tokens)
                (third tokens)
                (second tokens)
                (fourth tokens)))))

(display-lines-to-file (cons "#lang racket" (map format-line input))
                       "Output21.rkt")
