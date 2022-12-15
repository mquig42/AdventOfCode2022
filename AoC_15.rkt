;;;AoC_15.rkt
;;;2022-12-15
;;;Mike Quigley

;;;Another sensor/beacon puzzle, though simpler than the one that defeated me
;;;last year. There are only 32 sensors and 9 detected beacons. The difficulty
;;;is that they're spread over a very wide area, so checking every point
;;;would be too slow.

;;;Currently part 1 is done, but takes several minutes to check a single row
;;;Part 2 needs a more efficient approach.
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

;;Finds the number of excluded points on row, between xmin and xmax
;;Range includes xmin and excludes xmax
(define (excluded-count xmin xmax y sensors)
  (count identity
         (map (λ (x) (excluded-all? (make-point x y) sensors))
              (range xmin xmax))))

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
                         

(define input-file (open-input-file "Input15.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(- (excluded-count -3600000 8000000 2000000 input)
   (count-beacons 2000000 input))
