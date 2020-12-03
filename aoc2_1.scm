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
