#lang racket/base
(require racket/string racket/list racket/hash)

(define (main)
  (define args (current-command-line-arguments))
  (when (= (vector-length args) 0)
    (eprintf "Usage: racket 1brc.rkt <file>\n")
    (exit 1))

  (define file-path (vector-ref args 0))
  (define start (current-inexact-milliseconds))

  ;; Accumulate: station -> (vector min sum max count)
  (define table (make-hash))

  (call-with-input-file file-path
    (lambda (port)
      (let loop ()
        (define line (read-line port))
        (unless (eof-object? line)
          (define idx (find-char line #\;))
          (define station (substring line 0 idx))
          (define temp (string->number (substring line (add1 idx))))
          (define entry (hash-ref table station #f))
          (if entry
              (begin
                (vector-set! entry 0 (min (vector-ref entry 0) temp))
                (vector-set! entry 1 (+ (vector-ref entry 1) temp))
                (vector-set! entry 2 (max (vector-ref entry 2) temp))
                (vector-set! entry 3 (add1 (vector-ref entry 3))))
              (hash-set! table station (vector temp temp temp 1)))
          (loop)))))

  (define end (current-inexact-milliseconds))

  ;; Sort stations alphabetically
  (define sorted-stations (sort (hash-keys table) string<?))

  ;; Format output
  (define parts
    (for/list ([station (in-list sorted-stations)])
      (define entry (hash-ref table station))
      (define mn (vector-ref entry 0))
      (define sum (vector-ref entry 1))
      (define mx (vector-ref entry 2))
      (define cnt (vector-ref entry 3))
      (define mean (/ sum cnt))
      (format "~a=~a/~a/~a"
              station
              (format-1dp mn)
              (format-1dp mean)
              (format-1dp mx))))

  (printf "{~a}\n" (string-join parts ", "))
  (eprintf "Elapsed: ~a ms\n" (inexact->exact (round (- end start)))))

(define (find-char s ch)
  (let loop ([i 0])
    (cond
      [(= i (string-length s)) #f]
      [(char=? (string-ref s i) ch) i]
      [else (loop (add1 i))])))

(define (format-1dp x)
  (define v (/ (round (* (exact->inexact x) 10.0)) 10.0))
  (define whole (inexact->exact (truncate v)))
  (define frac (inexact->exact (abs (round (* (- v whole) 10)))))
  (when (= frac 10) (set! whole (+ whole (if (>= v 0) 1 -1))) (set! frac 0))
  (if (and (< v 0) (= whole 0))
      (format "-0.~a" frac)
      (format "~a.~a" whole frac)))

(main)
