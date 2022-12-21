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

;;;Update: optimized my program, now it can solve part 1 in seconds and part 2
;;;in about 10 minutes. I kept the output format, just to be different from
;;;my other programs. I also got rid of memoization, since it wasn't actually
;;;helping anything.
#lang racket

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

;;Getters for state
(define ore-robots first)
(define clay-robots second)
(define obsidian-robots third)
(define ore fourth)
(define clay fifth)
(define obsidian sixth)
(define geodes seventh)
(define m-rem eighth)
(define blueprint ninth)

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

;;Finds the maximum number of geodes that can be opened from given state
;;Memoize has been removed. States don't repeat,
;;so it was only slowing things down
(define (max-geodes state)
  (cond ((<= (m-rem state) 0) (geodes state))
        (else
         (max (max-geodes (build-ore state))
              (max-geodes (build-clay state))
              (max-geodes (build-obsidian state))
              (max-geodes (build-geode state))))))

;;Returns state after building an ore robot.
;;If the number of ore bots is already at maximum, returns a zero state
(define (build-ore state)
  (cond ((>= (ore-robots state) (max-ore (blueprint state))) zero-state)
        (else
         (let ((t (max 1 (+ 1 (ceiling
                               (/ (- (ore-ore (blueprint state)) (ore state))
                                  (ore-robots state)))))))
           (list (+ 1 (ore-robots state))
                 (clay-robots state)
                 (obsidian-robots state)
                 (- (+ (ore state) (* t (ore-robots state)))
                    (ore-ore (blueprint state)))
                 (+ (clay state) (* t (clay-robots state)))
                 (+ (obsidian state) (* t (obsidian-robots state)))
                 (geodes state)
                 (- (m-rem state) t)
                 (blueprint state))))))

;;Returns state after building a clay robot.
;;If the number of clay bots is already at maximum, returns a zero state
(define (build-clay state)
  (cond ((>= (clay-robots state) (max-clay (blueprint state))) zero-state)
        (else
         (let ((t (max 1 (+ 1 (ceiling
                               (/ (- (clay-ore (blueprint state)) (ore state))
                                  (ore-robots state)))))))
           (list (ore-robots state)
                 (+ 1 (clay-robots state))
                 (obsidian-robots state)
                 (- (+ (ore state) (* t (ore-robots state)))
                    (clay-ore (blueprint state)))
                 (+ (clay state) (* t (clay-robots state)))
                 (+ (obsidian state) (* t (obsidian-robots state)))
                 (geodes state)
                 (- (m-rem state) t)
                 (blueprint state))))))

;;Returns state after building an obsidian robot.
;;If the number of obsidian bots is already at maximum, or there are no clay
;;bots, returns a zero state
(define (build-obsidian state)
  (cond ((= (clay-robots state) 0) zero-state)
        ((>= (obsidian-robots state) (max-obsidian (blueprint state)))
         zero-state)
        (else
         (let ((t (max 1
                       (+ 1 (ceiling
                             (/ (- (obsidian-ore (blueprint state)) (ore state))
                                (ore-robots state))))
                       (+ 1 (ceiling
                             (/ (- (obsidian-clay (blueprint state))
                                   (clay state))
                                (clay-robots state)))))))
           (list (ore-robots state)
                 (clay-robots state)
                 (+ 1 (obsidian-robots state))
                 (- (+ (ore state) (* t (ore-robots state)))
                    (obsidian-ore (blueprint state)))
                 (- (+ (clay state) (* t (clay-robots state)))
                    (obsidian-clay (blueprint state)))
                 (+ (obsidian state) (* t (obsidian-robots state)))
                 (geodes state)
                 (- (m-rem state) t)
                 (blueprint state))))))

;;Returns state after building an obsidian robot.
;;If there are no obsidian bots, return a zero state
(define (build-geode state)
  (cond ((= (obsidian-robots state) 0) zero-state)
        (else
         (let ((t (max 1
                       (+ 1 (ceiling
                             (/ (- (geode-ore (blueprint state)) (ore state))
                                (ore-robots state))))
                       (+ 1 (ceiling
                             (/ (- (geode-obsidian (blueprint state))
                                   (obsidian state))
                                (obsidian-robots state)))))))
           (list (ore-robots state)
                 (clay-robots state)
                 (obsidian-robots state)
                 (- (+ (ore state) (* t (ore-robots state)))
                    (geode-ore (blueprint state)))
                 (+ (clay state) (* t (clay-robots state)))
                 (- (+ (obsidian state) (* t (obsidian-robots state)))
                    (geode-obsidian (blueprint state)))
                 (+ (geodes state) (- (m-rem state) t))
                 (- (m-rem state) t)
                 (blueprint state))))))

;;Returns a starting state with the given time and blueprint
(define (start-state minutes blueprint)
  (list 1 0 0 0 0 0 0 minutes blueprint))

;;This state evaluates to 0
(define zero-state (list 0 0 0 0 0 0 0 0 (list 99 99 99 99 99 99 99)))

;;Evaluates all blueprints with a 24-minute time span for part 1
(define (print-result minutes blueprint)
  (printf "Blueprint ~a can open ~a geodes~n"
          (blueprint-num blueprint)
          (max-geodes (start-state minutes blueprint))))

(define input-file (open-input-file "Input19.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1:\r")
(for-each (λ (x) (print-result 24 x)) input)
(display "\rPart 2:\r")
(for-each (λ (x) (print-result 32 x)) (take input 3))

