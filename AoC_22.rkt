;;;AoC_22.rkt
;;;2022-12-22
;;;Mike Quigley

;;;This looks like a difficult one. I see 2 main challenges here:
;;;1. represent the map in a way that allows wrapping
;;;2. Read and follow the instructions

;;;Reading the instructions was easier than I thought, thanks to regexes
;;;I'll store the direction (R or L) as an interned symbol instead of a string.
;;;That way I can use eq? to see which one it is, should be faster than string=?
#lang racket

(define input-lines (file->lines "Test22.txt"))
(define instrs (map (λ (x) (if (string->number x)
                               (string->number x)
                               (string->symbol x)))
                    (regexp-match* #px"\\d+|\\D" (last input-lines))))
