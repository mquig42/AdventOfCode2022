;;;AoC_11.rkt
;;;2022-12-11
;;;Mike Quigley

;;;A few thoughts: First, it doesn't matter in what order a monkey inspects
;;;its things. Everything in the queue will be looked at, one at a time
;;;This means I can use a stack instead of a queue.
;;;Second: I might want to use mutable vectors. They'll be faster.
;;;If I enclose all the mutable data inside a function, it's still FP, right?
;;;Third: I'm scared of part 2. This could be a day where part 2 is just
;;;"do part 1 a zillion times", and I'll need to find some math formula
;;;because conventional programming won't work on that scale.
#lang racket

;;The functions read-monkey, read-monkeys, operation, and test
;;are used for reading input and generating a list of monkeys
(define (read-monkeys file)
  (if (eof-object? (read-line file)) null
      (cons (read-monkey file) (read-monkeys file))))

(define (read-monkey file)
  (list (map string->number
             (string-split (string-trim (substring (read-line file) 18)) ", "))
        (operation (substring (read-line file) 19))
        (test (read-line file)
              (read-line file)
              (read-line file)
              (read-line file))))

;;Generates a procedure based on an Operation string from today's input
(define (operation str)
  (let ((parsed (string-split str)))
    (λ (old) ((if (string=? (second parsed) "*") * +)
              old
              (if (string=? (third parsed) "old")
                  old
                  (string->number (third parsed)))))))

;;Generates a procedure from the three Test lines in today's input
;;This procedure will take a worry level as input and return a monkey's number
;;str4 is not used, it's there so I can throw away the blank line between
;;monkeys in the puzzle input
(define (test str1 str2 str3 str4)
  (λ (n) (if (= 0 (modulo n (string->number (last (string-split str1)))))
             (string->number (last (string-split str2)))
             (string->number (last (string-split str3))))))

(define input-file (open-input-file "Input11.txt"))
(define input (read-monkeys input-file))
(close-input-port input-file)
