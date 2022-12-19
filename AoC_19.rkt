;;;AoC_19.rkt
;;;2022-12-19
;;;Mike Quigley

;;;I think we're getting sidetracked here. We fall off a bridge, try to get
;;;back in contact with the rest of the expedition, pick up a distress signal,
;;;follow it into a volcano, then have to find a way out. Makes sense so far.
;;;But now, we just notice some geodes and start building robots to crack them
;;;open, which requires building other robots first. What is this, Factorio?

;;;This seems a lot like day 16. Dynamic programming might work.
;;;On each turn, there are only 5 possible actions: build a robot or wait
;;;This is much less than the number of caves on day 16, but there's a lot of
;;;state data regarding how many robots have been built and resources collected.

;;;Because blueprints are evaluated separately, I can clear the cache
;;;after evaluating each one.

;;;Alternately, there may be a way to use math. Avoid just getting it from the
;;;subreddit. Maybe there's an article about calculating RTS build orders.
#lang racket
(require memo)

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-line line) (read-input file)))))

(define (parse-line line)
  (map string->number (regexp-match* #px"\\d+" line)))

;;Getters. Each factory is represented by a list of 7 numbers
;;If a speed boost is required, try using vectors instead
;;Naming scheme: the first word is the type of robot being built,
;;the second is the material used.
;;For example, clay-ore gets the amount of ore needed to build a clay robot
;;blueprint-num is the exception. It returns the blueprint number.
(define blueprint-num first)
(define ore-ore second)
(define clay-ore third)
(define obsidian-ore fourth)
(define obsidian-clay fifth)
(define geode-ore sixth)
(define geode-obsidian seventh)

(define input-file (open-input-file "Input19.txt"))
(define input (read-input input-file))
(close-input-port input-file)
