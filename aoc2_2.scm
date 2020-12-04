;; --- Day 2: Password Philosophy ---
;; --- Part Two ---

;; Assignment from https://adventofcode.com/2020/day/2#part2
;; Assignment author: Eric Wastl

;; While it appears you validated the passwords correctly, they don't
;; seem to be what the Official Toboggan Corporate Authentication
;; System is expecting.

;; The shopkeeper suddenly realizes that he just accidentally
;; explained the password policy rules from his old job at the sled
;; rental place down the street! The Official Toboggan Corporate
;; Policy actually works a little differently.

;; Each policy actually describes two positions in the password, where
;; 1 means the first character, 2 means the second character, and so
;; on. (Be careful; Toboggan Corporate Policies have no concept of
;; "index zero"!) Exactly one of these positions must contain the
;; given letter. Other occurrences of the letter are irrelevant for
;; the purposes of policy enforcement.

;; Given the same example list from above:

;;     1-3 a: abcde is valid: position 1 contains a and position 3 does not.
;;     1-3 b: cdefg is invalid: neither position 1 nor position 3 contains b.
;;     2-9 c: ccccccccc is invalid: both position 2 and position 9 contain c.

;; How many passwords are valid according to the new interpretation of
;; the policies?

(use-modules (ice-9 format) (ice-9 regex) (ice-9 match) (ice-9 rdelim))

(define cnt 0)

(define (do-passwd input)
  (define inp (string-tokenize input))
  (define range (string-split (car inp) #\-))
  (define range-start (string->number (car range)))
  (define range-end (string->number (cadr range)))
  (define letter (string-ref (cadr inp) 0))
  (define passwd (list-ref inp 2))
  (define char-count (string-count passwd letter))
  (if (eq? letter (string-ref passwd (- range-start 1)))
      (unless (eq? letter (string-ref passwd (- range-end 1)))
          (set! cnt (+ cnt 1))
          (format #t "~s\n" input))
      (if (eq? letter (string-ref passwd (- range-end 1)))
          (set! cnt (+ cnt 1))
          (format #t "~s\n" input))))

(define file (open-input-file "aoc2_input.txt"))

;; how the heck does this work?
(do ((line
      (read-line file)
      (read-line file)))
    ((eof-object? line))
  (do-passwd line))

(close-input-port file)

(format #t "\nSolution: ~d\n\n" cnt)
