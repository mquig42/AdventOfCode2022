;;;AoC_07.rkt
;;;2022-12-07
;;;Mike Quigley

;;;Need to build a map of some filesystem based on a command log
;;;Basic rules: Any line starting with $ is a command, either cd or ls
;;;Anything else is the output of an ls command. Directories start with dir,
;;;files start with a size.

;;;Part 1 only asks for total size of a directory, but part 2 may involve
;;;individual files, so best to put them in the map too.

;;;I needed to use mutable data for this one. Hash tables and sets are both
;;;mutable and resizable, so they'll work nicely.
;;;A dir is represented by a hash table containing the following:
;;;name:   string
;;;parent: dir
;;;dirs:   hash table of dirs, name as key
;;;files:  hash table of file sizes, name as key

;;;I also used a set, all-dirs, containing every dir without a tree structure
;;;so I could easily iterate over each dir
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string-trim line) (read-input file)))))

;;Create a hash table representing a new dir
(define (make-dir name parent)
  (let ((new-dir (make-hash)))
    (hash-set! new-dir "name" name)
    (hash-set! new-dir "parent" parent)
    (hash-set! new-dir "dirs" (make-hash))
    (hash-set! new-dir "files" (make-hash))
    new-dir))

;;Build a filesystem map
(define (map-files dir input)
  (if (null? input) (display "Filesystem map complete\r")
      (let ((line (car input)))
        (cond ((string=? line "$ ls")       ;For ls, do nothing.
               (map-files dir (cdr input)))
              ((string=? line "$ cd ..")    ;Go to parent dir
               (map-files (hash-ref dir "parent") (cdr input)))
              ((string-prefix? line "$ cd") ;Go to child dir
               (map-files (hash-ref (hash-ref dir "dirs")
                                    (substring line 5))
                          (cdr input)))
              ((string-prefix? line "dir")  ;Create child dir
               (let ((new-dir (make-dir (substring line 4) dir)))
                 (hash-set! (hash-ref dir "dirs")
                            (substring line 4)
                            new-dir)
                 (set-add! all-dirs new-dir)
                 (map-files dir (cdr input))))
              (else                         ;Create file
               (hash-set! (hash-ref dir "files")
                          (second (string-split line))
                          (string->number (first (string-split line))))
               (map-files dir (cdr input)))))))

;;Returns the size of a dir
(define (dir-size dir)
  (+ (foldl + 0 (hash-values (hash-ref dir "files")))
     (foldl + 0 (map dir-size (hash-values (hash-ref dir "dirs"))))))

;;Given a sorted list of numbers, find the first number larger than tgt
(define (find-tgt-size lst tgt)
  (if (>= (car lst) tgt) (car lst)
      (find-tgt-size (cdr lst) tgt)))
  

(define input-file (open-input-file "Input07.txt"))
(define input (read-input input-file))
(close-input-port input-file)

(define root (make-dir "/" null))
(define all-dirs (mutable-set))
(set-add! all-dirs root)
(map-files root (cdr input))

(define dir-sizes (map dir-size (set->list all-dirs)))

(display "Part 1: ")
(foldl + 0 (filter (λ (x) (<= x 100000)) dir-sizes))
(display "Part 2: ")
(find-tgt-size (sort dir-sizes <) (- 30000000 (- 70000000 (dir-size root))))
