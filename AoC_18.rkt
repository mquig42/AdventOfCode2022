;;;AoC_18.rkt
;;;2022-12-18
;;;Mike Quigley

;;;Finally, something I can solve after the last 2 days
;;;Part 1 is based on set intersections. Just count the number of adjacent
;;;cubes and subtract that from 6 to get surface area.
;;;Part 2 is more difficult. I need to find the outside surface area,
;;;not counting any fully enclosed air pockets.
;;;The good news is that this is a fairly small object. Every coordinate
;;;value is between 0 and 19. Total volume of that space is only 8000.
;;;Some sort of flood-fill approach might work.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) (set)
        (set-add (read-input file) (parse-line (string-trim line))))))

(define (parse-line line)
  (map string->number (string-split line ",")))

;;Each cube is represented by a list containing its x, y, and z coords
(define (make-cube x y z)
  (list x y z))
(define (get-x cube)
  (first cube))
(define (get-y cube)
  (second cube))
(define (get-z cube)
  (third cube))

;;Makes a set containing 6 points adjacent to cube
(define (enumerate-neighbours cube)
  (let ((x (get-x cube))
        (y (get-y cube))
        (z (get-z cube)))
    (set (make-cube (+ x 1) y z)
         (make-cube (- x 1) y z)
         (make-cube x (+ y 1) z)
         (make-cube x (- y 1) z)
         (make-cube x y (+ z 1))
         (make-cube x y (- z 1)))))

;;Gets exposed surface area for a single cube. This will be a number from 0 to 6
(define (surface-area cube cubes)
  (- 6 (set-count (set-intersect (enumerate-neighbours cube) cubes))))

;;Solves part 1. Finds total exposed surface area of all cubes.
(define (total-surface-area cubes)
  (foldl + 0 (map (λ (c) (surface-area c cubes)) (set->list cubes))))

;;Returns a list containing the minimum and maximum values for each axis
;;extended by one in each direction
(define (get-extents cubes)
  (let ((cubes-lst (set->list cubes)))
    (list (- (argmin identity (map get-x cubes-lst)) 1)
          (+ (argmax identity (map get-x cubes-lst)) 1)
          (- (argmin identity (map get-y cubes-lst)) 1)
          (+ (argmax identity (map get-y cubes-lst)) 1)
          (- (argmin identity (map get-z cubes-lst)) 1)
          (+ (argmax identity (map get-z cubes-lst)) 1))))

;;Return the minimum point of a set.
(define (get-min cubes)
  (let ((cubes-lst (set->list cubes)))
    (make-cube (argmin identity (map get-x cubes-lst))
               (argmin identity (map get-y cubes-lst))
               (argmin identity (map get-z cubes-lst)))))

;;Returns a set containing every cube within the given extents
(define (enumerate-volume extents)
  (define (iter x y z x-min x-max y-min y-max z-min z-max acc)
    (cond ((> z z-max)
           (iter x (+ y 1) z-min x-min x-max y-min y-max z-min z-max acc))
          ((> y y-max)
           (iter (+ x 1) y-min z-min x-min x-max y-min y-max z-min z-max acc))
          ((> x x-max) acc)
          (else
           (iter x y (+ z 1)  x-min x-max y-min y-max z-min z-max
                 (set-add acc (make-cube x y z))))))
  (iter (first extents)
        (third extents)
        (fifth extents)
        (first extents)
        (second extents)
        (third extents)
        (fourth extents)
        (fifth extents)
        (sixth extents)
        (set)))

;;Breadth first flood fill
(define (flood boundary to-flood flooded)
  (cond ((set-empty? to-flood) flooded)
        (else
         (let* ((p (set-first to-flood))
                (p-neighbours (set-subtract
                               (set-subtract
                                (enumerate-neighbours p)
                                boundary)
                               flooded)))
           (flood boundary
                  (set-remove (set-union to-flood p-neighbours) p)
                  (set-add flooded p))))))

(define input-file (open-input-file "Input18.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(display "Part 1: ")
(total-surface-area input)

;;Now do some set operations for part 2
(define volume (enumerate-volume (get-extents input)))
(define not-lava (set-subtract volume input))
(define boundary
  (set-subtract (enumerate-volume (get-extents not-lava)) not-lava))
(define flooded (flood boundary (set (get-min not-lava)) (set)))
(define air-pockets (set-subtract not-lava flooded))

(display "Part 2: ")
(total-surface-area (set-union input air-pockets))

