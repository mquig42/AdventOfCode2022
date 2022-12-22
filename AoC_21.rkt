;;;AoC_21.rkt
;;;2022-12-21
;;;Mike Quigley

;;;Instead of solving the problem,
;;;I can reformat the input into a program and then run it
#lang racket
;;Note: the following two functions were used to get my original answer to
;;part 1. I have since written another way to do that, which is used instead
;;when this program is run.
;;Convert a line of input to Racket code
(define (format-line line)
  (let ((tokens (string-split (string-replace line ":" ""))))
    (if (= 2 (length tokens))
        (format "(define (~a) ~a)" (first tokens) (second tokens))
        (format "(define (~a) (~a (~a) (~a)))"
                (first tokens)
                (third tokens)
                (second tokens)
                (fourth tokens)))))

;;Writes a file containing the puzzle input, translated to Racket
(define (make-output-file)
  (display-lines-to-file (cons "#lang racket" (map format-line input))
                         "Output21.rkt"))

(define (string->oper s)
  (cond ((string=? s "+") +)
        ((string=? s "-") -)
        ((string=? s "*") *)
        ((string=? s "/") /)))

;;Converts the puzzle input into a hash table representing all the monkeys
(define (make-expression-tree lines)
  (define (iter lines acc)
    (if (null? lines) acc
        (let ((tokens (string-split (string-replace (car lines) ":" ""))))
          (if (= 2 (length tokens))
              (iter (cdr lines)
                    (hash-set acc
                              (first tokens)
                              (string->number (second tokens))))
              (iter (cdr lines)
                    (hash-set acc
                              (first tokens)
                              (list (string->oper (third tokens))
                                    (second tokens)
                                    (fourth tokens))))))))
  (iter lines (make-immutable-hash)))

;;Evaluates the numerical value of a given monkey
(define (eval-tree tree monkey humn-value)
  (let ((monkey-val (hash-ref tree monkey)))
    (cond ((string=? monkey "humn") humn-value)
          ((number? monkey-val) monkey-val)
          (else ((first monkey-val)
                 (eval-tree tree (second monkey-val) humn-value)
                 (eval-tree tree (third monkey-val) humn-value))))))

;;Does the given monkey's value depend on humn?
(define (depends-humn? tree monkey)
  (let ((monkey-val (hash-ref tree monkey)))
    (cond ((string=? monkey "humn") true)
          ((number? monkey-val) false)
          (else (or (depends-humn? tree (second monkey-val))
                    (depends-humn? tree (third monkey-val)))))))

;;If a monkey's answer does not depend on humn, replace it with the number
(define (simplify-1 tree humn-value)
  (define (iter lst acc)
    (cond ((null? lst) acc)
          ((depends-humn? tree (car lst)) (iter (cdr lst) acc))
          (else (iter (cdr lst)
                      (hash-set acc (car lst)
                                (eval-tree tree (car lst) humn-value))))))
  (iter (hash-keys tree) tree))

(define input (file->lines "Input21.txt"))
(define monkey-tree (make-expression-tree input))
(define simplified-tree-1 (simplify-1 monkey-tree 
                                      (hash-ref monkey-tree "humn")))
;Applying simplify-1 reduces the number of active monkeys from 1147 to 70

(display "Part 1: ")
(eval-tree monkey-tree "root" (hash-ref monkey-tree "humn"))

(display "Part 2: ")
(define monkeys2
  (hash-set monkey-tree
            "root"
            (list-set (hash-ref monkey-tree "root") 0 -)))
;Set root to use -. That way, if the two params are equal, it will evaluate
;to 0. At this point, I just called eval-tree a bunch of times with different
;humn-values until I found one that got me a 0.