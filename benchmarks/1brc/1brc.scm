#!/usr/bin/env guile
!#

(use-modules (ice-9 hash-table)
             (ice-9 rdelim)
             (ice-9 format))

(define (round-to-1dp x)
  (/ (round (* x 10.0)) 10.0))

(define (format-temp x)
  (let ((r (round-to-1dp x)))
    (format #f "~,1f" r)))

(define (main args)
  (when (< (length args) 2)
    (display "Usage: guile --no-auto-compile 1brc.scm <file>\n")
    (exit 1))

  (let* ((file-path (cadr args))
         (stations (make-hash-table 500))
         (start-time (get-internal-real-time))
         (port (open-input-file file-path)))

    ;; Read all lines and accumulate stats
    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (let* ((semi-pos (string-index line #\;))
               (name (substring line 0 semi-pos))
               (temp (string->number (substring line (+ semi-pos 1))))
               (entry (hash-ref stations name)))
          (if entry
              (begin
                (vector-set! entry 0 (min (vector-ref entry 0) temp))
                (vector-set! entry 1 (max (vector-ref entry 1) temp))
                (vector-set! entry 2 (+ (vector-ref entry 2) temp))
                (vector-set! entry 3 (+ (vector-ref entry 3) 1)))
              (hash-set! stations name (vector temp temp temp 1))))
        (loop (read-line port))))

    (close-input-port port)

    ;; Collect and sort station names
    (let* ((names (sort (hash-map->list (lambda (k v) k) stations) string<?))
           (parts (map (lambda (name)
                         (let* ((entry (hash-ref stations name))
                                (mn (vector-ref entry 0))
                                (mx (vector-ref entry 1))
                                (sum (vector-ref entry 2))
                                (cnt (vector-ref entry 3))
                                (mean (/ sum cnt)))
                           (string-append name "="
                                          (format-temp mn) "/"
                                          (format-temp mean) "/"
                                          (format-temp mx))))
                       names))
           (end-time (get-internal-real-time))
           (elapsed-ms (* 1000.0 (/ (- end-time start-time)
                                     internal-time-units-per-second))))

      (display "{")
      (display (string-join parts ", "))
      (display "}")
      (newline)

      (format #t "Elapsed: ~,1f ms~%" elapsed-ms))))

(main (command-line))
