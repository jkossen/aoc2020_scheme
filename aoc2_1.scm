;; --- Day 2: Password Philosophy ---
;; --- Part One ---

;; Your flight departs in a few days from the coastal airport; the
;; easiest way down to the coast from here is via toboggan.

;; The shopkeeper at the North Pole Toboggan Rental Shop is having a
;; bad day. "Something's wrong with our computers; we can't log in!"
;; You ask if you can take a look.

;; Their password database seems to be a little corrupted: some of the
;; passwords wouldn't have been allowed by the Official Toboggan
;; Corporate Policy that was in effect when they were chosen.

;; To try to debug the problem, they have created a list (your puzzle
;; input) of passwords (according to the corrupted database) and the
;; corporate policy when that password was set.

;; For example, suppose you have the following list:

;; 1-3 a: abcde
;; 1-3 b: cdefg
;; 2-9 c: ccccccccc

;; Each line gives the password policy and then the password. The
;; password policy indicates the lowest and highest number of times a
;; given letter must appear for the password to be valid. For example,
;; 1-3 a means that the password must contain a at least 1 time and at
;; most 3 times.

;; In the above example, 2 passwords are valid. The middle password,
;; cdefg, is not; it contains no instances of b, but needs at least
;; 1. The first and third passwords are valid: they contain one a or
;; nine c, both within the limits of their respective policies.

;; How many passwords are valid according to their policies?

(use-modules (ice-9 format) (ice-9 regex) (ice-9 match) (ice-9 rdelim))

(define cnt 0)

(define (do-passwd input)
  (define inp (string-tokenize input))
  (define range (string-split (car inp) #\-))
  (define range-start (string->number (car range)))
  (define range-end (string->number (cadr range)))
  ;; this looks ugly, is there no string->char?
  ;;(define letter (car (string->list (string-take (cadr inp) 1))))
  ;; this works:
  (define letter (string-ref (cadr inp) 0))
  (define passwd (list-ref inp 2))
  (define char-count (string-count passwd letter))
  (if (>= char-count range-start)
      (if (<= char-count range-end)
          (set! cnt (+ cnt 1))
          (format #t "~s\n" input))))

(define file (open-input-file "aoc2_input.txt"))

;; how the heck does this work?
(do ((line (read-line file) (read-line file))) ((eof-object? line))
  (do-passwd line))

(close-input-port file)

(format #t "\nSolution: ~d\n\n" cnt)
