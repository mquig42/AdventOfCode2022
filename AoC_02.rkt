;;;AoC_02.rkt
;;;2022-12-02
;;;Mike Quigley

;;;Today we're scoring a rock paper scissors tournament.
;;;Nothing too challenging, but writing out the score for every case
;;;did get a bit tedious.
;;;For part 2, the interpretation of the input is different, so I just
;;;copied and rearranged my scoring function.
#lang racket

(define (read-input file)
  (define (iter lst)
    (let ((line (read-line file)))
      (if (eof-object? line) lst
          (iter (cons (string-split line) lst)))))
  (iter null))

(define (score1 round)
  (cond ((string=? (cadr round) "X")
         (cond ((string=? (car round) "A") 4)     ;Rock draw
               ((string=? (car round) "B") 1)     ;Rock lose
               ((string=? (car round) "C") 7)))   ;Rock win
        ((string=? (cadr round) "Y")
         (cond ((string=? (car round) "A") 8)     ;Paper win
               ((string=? (car round) "B") 5)     ;Paper draw
               ((string=? (car round) "C") 2)))   ;Paper lose
        ((string=? (cadr round) "Z")
         (cond ((string=? (car round) "A") 3)     ;Scissors lose
               ((string=? (car round) "B") 9)     ;Scissors win
               ((string=? (car round) "C") 6))))) ;Scissors draw

(define (score2 round)
  (cond ((string=? (cadr round) "X")
         (cond ((string=? (car round) "A") 3)     ;Scissors lose
               ((string=? (car round) "B") 1)     ;Rock lose
               ((string=? (car round) "C") 2)))   ;Paper lose
        ((string=? (cadr round) "Y")
         (cond ((string=? (car round) "A") 4)     ;Rock draw
               ((string=? (car round) "B") 5)     ;Paper draw
               ((string=? (car round) "C") 6)))   ;Scissors draw
        ((string=? (cadr round) "Z")
         (cond ((string=? (car round) "A") 8)     ;Paper win
               ((string=? (car round) "B") 9)     ;Scissors win
               ((string=? (car round) "C") 7))))) ;Rock win

(define input-file (open-input-file "Input02.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl + 0 (map score1 input))
(display "Part 2: ")
(foldl + 0 (map score2 input))