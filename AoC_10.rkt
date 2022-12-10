;;;AoC_10.rkt
;;;2022-12-10
;;;Mike Quigley

;;;This is too easy...
;;;The input is only 139 lines. I could probably solve this by hand.
;;;What will part 2 bring, I wonder?

;;;Update: Oh, it's one of these puzzles where the output spells out
;;;a word in giant letters. Those are always fun. I'll finish in the morning,
;;;but it looks like my approach to part 1 of making a vector containing
;;;the value of the x register during every cycle will set me up well for part 2
;;;Update: Had to tweak things a bit. Switched from a vector to a list.
;;;It makes part 1 less efficient, but is better for part 2
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (cond ((eof-object? line) null)
          ((string=? line "noop\r") (cons "noop" (read-input file)))
          (else (cons
                 (string->number (second (string-split line)))
                 (read-input file))))))

;;Makes a list containing the value of the X register for each clock cycle
(define (plot-x instrs)
  (define (iter instrs x hist)
    (cond ((null? instrs) hist)
          ((eq? (car instrs) "noop") (iter (cdr instrs) x (cons x hist)))
          (else (iter (cdr instrs)
                      (+ x (car instrs))
                      (cons x (cons x hist))))))
  (reverse (iter instrs 1 '(0))))

;;Returns pixel value to draw based on x register and h counter
(define (crt-pixel h x)
  (if (<= (abs (- h x)) 1) #\▓ #\░))

;;Draws all the pixels defined by the sequence of x-register values.
;;h starts at 0
(define (draw-screen signals h)
  (cond ((null? signals) (void))
        ((= h 39)
         (display (crt-pixel h (car signals)))
         (display "│\r│")
         (draw-screen (cdr signals) 0))
        (else
         (display (crt-pixel h (car signals)))
         (draw-screen (cdr signals) (+ h 1)))))

(define input-file (open-input-file "Input10.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define signals (plot-x input))

(display "Part 1: ")
(foldl + 0 (map (λ (x) (* (list-ref signals x) x)) (range 20 221 40)))

(display "\rPart 2:\r")
(display "╭────────────────────────────────────────╮\r│")
(draw-screen (cdr signals) 0)
(display "│││││││││││││││││││││││││││││││││││││││││\r")
(display "╰┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴Elfdroid┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴┴╯")
