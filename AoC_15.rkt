;;;AoC_15.rkt
;;;2022-12-15
;;;Mike Quigley

;;;Another sensor/beacon puzzle, though simpler than the one that defeated me
;;;last year. There are only 32 sensors and 9 detected beacons. The difficulty
;;;is that they're spread over a very wide area, so checking every point
;;;would be too slow.

;;;Currently part 1 is done, but takes several minutes to check a single row
;;;Part 2 needs a more efficient approach.

;;;Update: Made a much faster approach to solving part 1. It went from
;;;90 seconds to under 1ms

;;;Have now solved both parts. Part 2 takes 66 seconds. May want to come back
;;;and use multithreading to reduce that further.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (parse-line line) (read-input file)))))

;;Each line of input contains one sensor and one beacon
;;Extract the coordinates as a list of two pairs
(define (parse-line line)
  (let ((numbers (map string->number (regexp-match* #px"[-\\d]+" line))))
    (list (cons (first numbers) (second numbers))
          (cons (third numbers) (fourth numbers)))))

;;Each point is a pair of coordinates
(define (make-point x y)
  (cons x y))
(define (get-x point)
  (car point))
(define (get-y point)
  (cdr point))

;;Returns Manhattan distance between two points
(define (manhattan-dist a b)
  (+ (abs (- (get-x a) (get-x b)))
     (abs (- (get-y a) (get-y b)))))

;;Every sensor is paired with its nearest beacon. This means that every sensor
;;has an exclusion zone which cannot contain another beacon, or else that other
;;beacon would be the closest.
;;Is the point within the sensor's exclusion zone?
(define (excluded? point sensor)
  (<= (manhattan-dist point (first sensor))
      (manhattan-dist (first sensor) (second sensor))))

;;Checks a point against all sensors
(define (excluded-all? point sensors)
  (cond ((null? sensors) false)
        ((excluded? point (car sensors)) true)
        (else (excluded-all? point (cdr sensors)))))

;;returns the minimum and maximum excluded x values for the given sensor
;;and y value, or false if the sensor's exclusion zone does not reach that y
(define (exclusion-range y sensor)
  (let* ((radius (manhattan-dist (first sensor) (second sensor)))
         (sensor-x (get-x (first sensor)))
         (y-dist (manhattan-dist (first sensor) (make-point sensor-x y)))
         (y-diff (- radius y-dist)))
    (if (> y-dist radius) false
        (list (- sensor-x y-diff) (+ sensor-x y-diff)))))

;;Returns a list of the exclusion ranges on y for all sensors
(define (exclusion-ranges-all y sensors)
  (sort (filter identity (map (λ (s) (exclusion-range y s)) sensors))
        (λ (a b) (< (first a) (first b)))))

;;Returns total number of excluded points on y, assuming no gap
(define (excluded-width y sensors)
  (let ((zones (exclusion-ranges-all y sensors)))
    (- (+ 1 (- (last (last zones)) (first (first zones))))
       (count-beacons y sensors))))

;;Returns the x-coord of any gap that may exist on y, or false
(define (find-x-gap y sensors)
  (define (iter xmax ranges)
    (cond ((null? ranges) false)
          ((< xmax (first (first ranges))) (+ xmax 1))
          (else (iter (max xmax (second (first ranges))) (cdr ranges)))))
  (let ((ranges (exclusion-ranges-all y sensors)))
    (if (null? ranges) false
        (iter (second (first ranges)) (cdr ranges)))))

;;Returns a point not within the exclusion zones of any sensor
(define (find-xy-gap sensors)
  (define (iter y)
    (let ((gap (find-x-gap y sensors)))
      (if gap (make-point gap y)
          (iter (+ y 1)))))
  (iter 0))

;;Counts detected beacons on row
(define (count-beacons y sensors)
  (define (iter sensors beacons)
    (cond ((null? sensors) (set-count beacons))
          ((= y (get-y (second (car sensors))))
           (iter (cdr sensors) (set-add beacons (second (car sensors)))))
          (else
           (iter (cdr sensors) beacons))))
  (iter sensors (set)))

;;Counts all detected beacons in input
(define (count-all-beacons sensors)
  (define (iter sensors beacons)
    (if (null? sensors) (set-count beacons)
        (iter (cdr sensors) (set-add beacons (second (car sensors))))))
  (iter sensors (set)))
                         
;(define mode '("Test15.txt" 10)) ;Test
(define mode '("Input15.txt" 2000000)) ;Prod
(define input-file (open-input-file (first mode)))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(excluded-width (second mode) input)

(define gap (find-xy-gap input))
(display "Gap found at: ")
gap
(display "Part 2: ")
(+ (* (get-x gap) 4000000) (get-y gap))
