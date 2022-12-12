;;;AoC_11.rkt
;;;2022-12-11
;;;Mike Quigley

;;;A few thoughts: First, it doesn't matter in what order a monkey inspects
;;;its things. Everything in the queue will be looked at, one at a time
;;;This means I can use a stack instead of a queue.
;;;Edit: Since the text refers to order mattering, I'm going to use queues
;;;even if it seems like stacks would also work
;;;Second: I might want to use mutable vectors. They'll be faster.
;;;If I enclose all the mutable data inside a function, it's still FP, right?
;;;Third: I'm scared of part 2. This could be a day where part 2 is just
;;;"do part 1 a zillion times", and I'll need to find some math formula
;;;because conventional programming won't work on that scale.

;;;Update: it was actually 10,000 times. The difficulty was that the
;;;"divide by 3" step was removed, so the numbers got too big.
;;;Solved that by modding by the product of all the test divisors
;;;(which I admit I got from the subreddit). My first attempt probably would
;;;have found the answer eventually. I estimate it would take at least 7 hours,
;;;and possibly several days.
;;;Which means I would have still gotten an answer before Christmas
#lang racket
(require queue)

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
;;Includes division by 3
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

;;Returns a vector containing each monkey's activity level after n rounds
;;This function uses mutable vectors and for-each internally,
;;but does not depend on any external state
(define (run-n-rounds monkeys rounds reduce reduce-val)
  (let ((queues (list->vector (map list->queue (map first monkeys))))
        (inspects (list->vector (map second monkeys)))
        (tests (list->vector (map third monkeys)))
        (activity (make-vector (length monkeys))))
    (define (monkey-turn n)
      (cond ((queue-empty? (vector-ref queues n)) (void))
            (else
             (define-values (elem new-q) (queue-remove (vector-ref queues n)))
             (let* ((new-val (reduce ((vector-ref inspects n) elem) reduce-val))
                    (new-idx ((vector-ref tests n) new-val)))
               (vector-set!
                queues
                new-idx
                (queue-add (vector-ref queues new-idx) new-val))
               (vector-set! queues n new-q)
               (vector-set! activity n (+ (vector-ref activity n) 1))
               (monkey-turn n)))))
    (for-each (λ (x) (for-each monkey-turn (range (length monkeys))))
              (range rounds))
    activity))

;;Makes a queue from a list.
(define (list->queue lst)
  (foldl (λ (x acc) (queue-add acc x)) (make-queue) lst))


(define input-file (open-input-file "Input11.txt"))
(define input (read-monkeys input-file))
(close-input-port input-file)

(display "Part 1: ")
(foldl * 1
       (vector->list
        (vector-take
         (vector-sort
          (run-n-rounds input 20 quotient 3) >) 2)))

;I'm using a hard-coded mod value here.
;TODO: write a function to find the mod value by going through the file again
(display "Part 2: ")
(foldl * 1
       (vector->list
        (vector-take
         (vector-sort
          (run-n-rounds input 10000 modulo 9699690) >) 2)))
