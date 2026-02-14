;;; 1 Billion Row Challenge — Gambit Scheme implementation
;;; Usage: gsi 1brc.gambit.scm /path/to/measurements.txt

(define (find-semicolon line)
  (let loop ((i 0))
    (cond
      ((= i (string-length line)) #f)
      ((char=? (string-ref line i) #\;) i)
      (else (loop (+ i 1))))))

(define (format-1dp x)
  (let* ((v (/ (round (* x 10.0)) 10.0))
         (whole (inexact->exact (truncate v)))
         (frac (inexact->exact (abs (round (* (- v whole) 10))))))
    (when (= frac 10)
      (set! whole (+ whole (if (>= v 0) 1 -1)))
      (set! frac 0))
    (if (and (< v 0) (= whole 0))
        (string-append "-0." (number->string frac))
        (string-append (number->string whole) "." (number->string frac)))))

(define (string-join lst sep)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;; Gambit's built-in sort crashes under x86_64 emulation, use merge sort
(define (merge-sort lst less?)
  (define (merge a b)
    (cond ((null? a) b)
          ((null? b) a)
          ((less? (car a) (car b)) (cons (car a) (merge (cdr a) b)))
          (else (cons (car b) (merge a (cdr b))))))
  (define (split lst)
    (let loop ((l lst) (a '()) (b '()) (toggle #t))
      (if (null? l) (values a b)
          (if toggle
              (loop (cdr l) (cons (car l) a) b #f)
              (loop (cdr l) a (cons (car l) b) #t)))))
  (if (or (null? lst) (null? (cdr lst))) lst
      (call-with-values (lambda () (split lst))
        (lambda (a b) (merge (merge-sort a less?) (merge-sort b less?))))))

(define (main)
  (let* ((args (command-line))
         ;; Gambit's gsi treats extra args as files to load, so we use
         ;; the BENCH_FILE env var instead of command-line args
         (file-path (or (getenv "BENCH_FILE")
                        (and (>= (length args) 2) (cadr args)))))
    (when (not file-path)
      (display "Usage: BENCH_FILE=<file> gsi 1brc.gambit.scm\n" (current-error-port))
      (exit 1))

    (let* ((file-path file-path)
           (table (make-table test: string=? hash: string=?-hash))
           (start-time (real-time))
           (port (open-input-file file-path)))

      (let loop ((line (read-line port)))
        (unless (eof-object? line)
          (let* ((semi (find-semicolon line))
                 (name (substring line 0 semi))
                 (temp (string->number (substring line (+ semi 1) (string-length line))))
                 (entry (table-ref table name #f)))
            (if entry
                (begin
                  (vector-set! entry 0 (min (vector-ref entry 0) temp))
                  (vector-set! entry 1 (+ (vector-ref entry 1) temp))
                  (vector-set! entry 2 (max (vector-ref entry 2) temp))
                  (vector-set! entry 3 (+ (vector-ref entry 3) 1)))
                (table-set! table name (vector temp temp temp 1))))
          (loop (read-line port))))

      (close-input-port port)

      (let* ((names '()))
        (table-for-each (lambda (k v) (set! names (cons k names))) table)
        (set! names (merge-sort names string<?))

        (let* ((parts (map (lambda (name)
                             (let* ((entry (table-ref table name))
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
               (end-time (real-time))
               (elapsed (inexact->exact (round (* (- end-time start-time) 1000)))))

          (display "{")
          (display (string-join parts ", "))
          (display "}")
          (newline)
          (display (string-append "Elapsed: " (number->string elapsed) " ms\n")
                   (current-error-port)))))))

(main)
