;;;AoC_07.rkt
;;;2022-12-07
;;;Mike Quigley

;;;Need to build a map of some filesystem based on a command log
;;;Basic rules: Any line starting with $ is a command, either cd or ls
;;;Anything else is the output of an ls command. Directories start with dir,
;;;files start with a size.

;;;Part 1 only asks for total size of a directory, but part 2 may involve
;;;individual files, so best to put them in the map too.

;;;I think I'll need to use mutable data for this, at least within the
;;;function that generates the filesystem map.

;;;I'll need two datatypes (structs?)
;;;File is just a list of two elements: name and size
;;;Dir must contain name, parent, list of subdirs, list of files
;;;The lists must be mutable and resizable.
#lang racket

(define (read-input file)
  (let ((line (read-line file)))
    (if (eof-object? line) null
        (cons (string-trim line) (read-input file)))))

(define input-file (open-input-file "Input07.txt"))
(define input (read-input input-file))
(close-input-port input-file)
