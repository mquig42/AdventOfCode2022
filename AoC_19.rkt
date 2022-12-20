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

;;;For part 1, the program is slow enough (and doesn't free memory properly)
;;;that I don't calculate the answer. Instead I print something like
;;;Blueprint 14 can open 4 geodes
;;;and do the rest of the work by hand. This means I don't need to test
;;;every blueprint in a single run.
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

;;Maximum required production of each material. Can only build one robot
;;per minute, so no point having more resource production than the most
;;expensive robot
(define (max-ore blueprint)
  (max (ore-ore blueprint)
       (clay-ore blueprint)
       (obsidian-ore blueprint)
       (geode-ore blueprint)))
(define max-clay obsidian-clay)
(define max-obsidian geode-obsidian)

;;Test if it's impossible to build each robot type
(define (ore? blueprint ore)
  (< ore (ore-ore blueprint)))
(define (clay? blueprint ore)
  (< ore (clay-ore blueprint)))
(define (obsidian? blueprint ore clay)
  (or (< ore (obsidian-ore blueprint))
      (< clay (obsidian-clay blueprint))))
(define (geode? blueprint ore obsidian)
  (or (< ore (geode-ore blueprint))
      (< obsidian (geode-obsidian blueprint))))

(define/memoize (max-geodes state) #:hash hash
  (let* ((ore-robots (first state))
         (clay-robots (second state))
         (obsidian-robots (third state))
         (ore (fourth state))
         (clay (fifth state))
         (obsidian (sixth state))
         (geodes (seventh state))
         (m-rem (eighth state))
         (blueprint (ninth state))
         (ore-next (+ ore ore-robots))
         (clay-next (+ clay clay-robots))
         (obsidian-next (+ obsidian obsidian-robots))
         (time-next (- m-rem 1)))
    (cond ((= m-rem 0) geodes)
          (else
           (max (max-geodes (list ore-robots ;;Don't build any robots
                                  clay-robots
                                  obsidian-robots
                                  ore-next
                                  clay-next
                                  obsidian-next
                                  geodes
                                  time-next
                                  blueprint))
                (if (or (ore? blueprint ore)
                        (>= ore-robots (max-ore blueprint)))
                    0
                    (max-geodes (list (+ ore-robots 1) ;;Build ore robot
                                      clay-robots
                                      obsidian-robots
                                      (- ore-next (ore-ore blueprint))
                                      clay-next
                                      obsidian-next
                                      geodes
                                      time-next
                                      blueprint)))
                (if (or (clay? blueprint ore)
                        (>= clay-robots (max-clay blueprint)))
                    0
                    (max-geodes (list ore-robots ;;Build clay robot
                                      (+ clay-robots 1)
                                      obsidian-robots
                                      (- ore-next (clay-ore blueprint))
                                      clay-next
                                      obsidian-next
                                      geodes
                                      time-next
                                      blueprint)))
                (if (or (obsidian? blueprint ore clay)
                        (>= obsidian-robots (max-obsidian blueprint)))
                    0
                    (max-geodes (list ore-robots ;;Build obsidian robot
                                      clay-robots
                                      (+ obsidian-robots 1)
                                      (- ore-next (obsidian-ore blueprint))
                                      (- clay-next (obsidian-clay blueprint))
                                      obsidian-next
                                      geodes
                                      time-next
                                      blueprint)))
                (if (geode? blueprint ore obsidian)
                    0
                    (max-geodes (list ore-robots ;;Build geode robot
                                      clay-robots
                                      obsidian-robots
                                      (- ore-next (geode-ore blueprint))
                                      clay-next
                                      (- obsidian-next
                                         (geode-obsidian blueprint))
                                      (+ geodes time-next)
                                      time-next
                                      blueprint))))))))
                           
(define (start-state blueprint)
  (list 1 0 0 0 0 0 0 24 blueprint))

(define (print-result blueprint)
  (printf "Blueprint ~a can open ~a geodes~n"
          (blueprint-num blueprint)
          (max-geodes (start-state blueprint)))
  (set-box! (max-geodes) (hash))
  (collect-garbage))

(define input-file (open-input-file "Input19.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1:\r")
(for-each print-result (take input 10))
