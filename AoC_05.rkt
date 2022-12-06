;;;AoC_05.rkt
;;;2022-12-05
;;;Mike Quigley

;;;To solve this, I made some data-handling functions to manage a list of stacks
;;;Part 2 was easy enough to adapt. Just write a new function for moving
;;;n crates from one stack to another. The rest is the same as part 1.
#lang racket

(define (read-crates file)
  (let ((line (read-line file)))
    (if (string=? line "\r") null
        (cons (map (λ (x) (string-ref line x))
                   (range 1 (string-length line) 4))
              (read-crates file)))))

(define (read-instructions file)
  (define (parse-instruction inst)
    (filter identity (map string->number (string-split inst))))
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-instruction line) (read-instructions file)))))

;;Create a list of empty stacks
(define (stacks-init max-index)
  (define (iter n stacks)
    (if (= n 0) stacks
        (iter (- n 1) (cons null stacks))))
  (iter max-index (cons null null)))

;;Adds a value to the stack at index
(define (stacks-put stacks index value)
  (list-set stacks index
            (cons value (list-ref stacks index))))

;;Adds a line of values to stacks
(define (stacks-put-line stacks line)
  (define (iter stacks index line)
    (cond ((null? line)
           stacks)
          ((eq? (car line) #\space)
           (iter stacks (+ index 1) (cdr line)))
          (else
           (iter (stacks-put stacks index (car line)) (+ index 1) (cdr line)))))
  (iter stacks 1 line))

;;Puts the initial state from read-crates into stacks
(define (stacks-put-crates stacks crates)
  (if (null? crates) stacks
      (stacks-put-line (stacks-put-crates stacks (cdr crates)) (car crates))))

;;Move a single value from one stack to another
(define (stacks-move stacks from-index to-index)
  (let ((from-stack (list-ref stacks from-index))
        (to-stack (list-ref stacks to-index)))
    (list-set (list-set stacks from-index (cdr from-stack))
              to-index (cons (car from-stack) to-stack))))

;;Move n values from one stack to another
(define (process-instruction stacks n from to)
  (if (= n 0) stacks
      (process-instruction (stacks-move stacks from to)
                           (- n 1) from to)))

;;Move n values from one stack to another, following part 2's rules
(define (process-instruction-2 stacks n from-index to-index)
  (let ((from-stack (list-ref stacks from-index))
        (to-stack (list-ref stacks to-index)))
    (list-set (list-set stacks from-index (drop from-stack n))
              to-index (append (take from-stack n) to-stack))))

;;Return the resulting stacks after following all instructions
(define (process-all-instructions method stacks instructions)
  (if (null? instructions) stacks
      (let ((inst (car instructions)))
        (process-all-instructions method
         (method stacks (first inst) (second inst) (third inst))
         (cdr instructions)))))

(define input-file (open-input-file "Input05.txt"))
(define crates (read-crates input-file))
(define instructions (read-instructions input-file))
(close-input-port input-file)

(define stacks (stacks-put-crates (stacks-init (length (car crates))) crates))

(display "Part 1: ")
(list->string (map car (cdr (process-all-instructions
                             process-instruction stacks instructions))))
(display "Part 2: ")
(list->string (map car (cdr (process-all-instructions
                             process-instruction-2 stacks instructions))))
