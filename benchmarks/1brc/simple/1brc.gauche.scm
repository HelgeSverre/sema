;;; 1BRC — Gauche (simple/idiomatic version)
;;; Usage: gosh 1brc.gauche.scm /path/to/measurements.txt

(use srfi.13)    ; string-index, string-join
(use scheme.sort) ; list-sort
(use srfi.19)    ; current-time

(define (current-ms)
  (let ((t (current-time)))
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

(define (format-1dp x)
  (let* ((v (/ (round (* x 10.0)) 10.0))
         (whole (inexact->exact (truncate v)))
         (frac (inexact->exact (abs (round (* (- v whole) 10))))))
    (when (= frac 10)
      (set! whole (+ whole (if (>= v 0) 1 -1)))
      (set! frac 0))
    (if (and (< v 0) (= whole 0))
        (format #f "-0.~a" frac)
        (format #f "~a.~a" whole frac))))

(define (main args)
  (when (< (length args) 2)
    (format (current-error-port) "Usage: gosh 1brc.gauche.scm <file>~%")
    (exit 1))

  (let* ((file-path (cadr args))
         (table (make-hash-table 'string=?))
         (start-time (current-ms))
         (port (open-input-file file-path)))

    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (let* ((semi (string-index line #\;))
               (name (substring line 0 semi))
               (temp (string->number (substring line (+ semi 1) (string-length line))))
               (entry (hash-table-get table name #f)))
          (if entry
              (begin
                (vector-set! entry 0 (min (vector-ref entry 0) temp))
                (vector-set! entry 1 (+ (vector-ref entry 1) temp))
                (vector-set! entry 2 (max (vector-ref entry 2) temp))
                (vector-set! entry 3 (+ (vector-ref entry 3) 1)))
              (hash-table-put! table name (vector temp temp temp 1))))
        (loop (read-line port))))

    (close-input-port port)

    (let* ((names (list-sort string<? (hash-table-keys table)))
           (parts (map (lambda (name)
                         (let* ((entry (hash-table-get table name))
                                (mn (vector-ref entry 0))
                                (sum (vector-ref entry 1))
                                (mx (vector-ref entry 2))
                                (cnt (vector-ref entry 3))
                                (mean (/ sum cnt)))
                           (string-append name "="
                                          (format-1dp mn) "/"
                                          (format-1dp mean) "/"
                                          (format-1dp mx))))
                       names))
           (end-time (current-ms))
           (elapsed (- end-time start-time)))

      (format #t "{~a}~%" (string-join parts ", "))
      (format (current-error-port) "Elapsed: ~a ms~%" elapsed)
      0)))

; Gauche auto-calls (main) at exit when defined
