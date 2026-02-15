;;; 1 Billion Row Challenge — Gauche Scheme implementation
;;; Usage: gosh 1brc.gauche.scm /path/to/measurements.txt

(use srfi.13) ; string-index, string-join
(use scheme.sort) ; list-sort
(use srfi.19) ; current-time

(define (current-ms)
  (let ((t (current-time)))
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

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

(define (format-1dp n)
  (let* ((neg (< n 0))
         (abs-n (abs n))
         (whole (quotient abs-n 10))
         (frac (remainder abs-n 10)))
    (if (and neg (= whole 0))
        (format #f "-0.~a" frac)
        (if neg
            (format #f "-~a.~a" whole frac)
            (format #f "~a.~a" whole frac)))))

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
               (temp (parse-temp-int10 line (+ semi 1)))
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
                                (mean (inexact->exact (round (/ (* sum 1.0) cnt)))))
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
