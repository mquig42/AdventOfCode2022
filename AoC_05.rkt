;;;AoC_05.rkt
;;;2022-12-05
;;;Mike Quigley

;;;There are three challenges here
;;;1. Parse the input
;;;2. Follow the stack-rearranging instructions
;;;3. Whatever part 2 ends up being

;;;For parsing, use something like
;;;(map (λ (x) (string-ref line x)) (range 1 (string-length line) 4))
;;;to parse out the crates from each line of input text
;;;Explanation: crates occur at index 1, and then every 4 after that until
;;;the end of the line. Generate a range, and then map string-ref over it
;;;After doing that, need to turn the whole thing on its side to get
;;;a list of stacks (can use lists as stacks)

;;;Parsing and following the instructions should be relatively easy.
;;;No idea what part 2 will bring. Going to bed now,
;;;this won't be solved quickly

#lang racket
(define (read-crates file)
  (let ((line (read-line file)))
    (if (string=? line "\r") null
        (cons (map (λ (x) (string-ref line x))
                   (range 1 (string-length line) 4))
              (read-crates file)))))

;;TODO actually parse the instructions. This just reads strings.
(define (read-instructions file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons line (read-instructions file)))))

(define input-file (open-input-file "Test05.txt"))
(define crates (read-crates input-file))
(define instructions (read-instructions input-file))
(close-input-port input-file)

crates