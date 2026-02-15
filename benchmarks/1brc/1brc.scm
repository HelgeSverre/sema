#!/usr/bin/env guile
!#

(use-modules (ice-9 hash-table)
             (ice-9 rdelim)
             (ice-9 format))

;; Parse temperature as integer×10 directly from line, starting at index `start`.
;; "12.3" → 123, "-5.1" → -51. Avoids substring allocation and string->number.
(define (parse-temp-int10 line start)
  (let* ((len (string-length line))
         (neg? (char=? (string-ref line start) #\-))
         (i (if neg? (+ start 1) start)))
    (let loop ((i i) (acc 0))
      (if (>= i len)
          (if neg? (- acc) acc)
          (let ((c (string-ref line i)))
            (if (char=? c #\.)
                (loop (+ i 1) acc)
                (loop (+ i 1) (+ (* acc 10) (- (char->integer c) 48)))))))))

;; Format an integer×10 value as a decimal string: 123 → "12.3", -51 → "-5.1"
(define (format-temp n)
  (let* ((sign (if (negative? n) "-" ""))
         (a (abs n))
         (whole (quotient a 10))
         (frac (remainder a 10)))
    (string-append sign (number->string whole) "." (number->string frac))))

(define (main args)
  (when (< (length args) 2)
    (display "Usage: guile 1brc.scm <file>\n")
    (exit 1))

  (let* ((file-path (cadr args))
         (stations (make-hash-table 500))
         (start-time (get-internal-real-time))
         (port (open-input-file file-path)))

    ;; Read all lines and accumulate stats (integer×10)
    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (let* ((semi-pos (string-index line #\;))
               (name (substring line 0 semi-pos))
               (temp (parse-temp-int10 line (+ semi-pos 1)))
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
                                (mean (inexact->exact (round (/ (* sum 1.0) cnt)))))
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
