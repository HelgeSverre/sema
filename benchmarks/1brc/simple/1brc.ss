#!/usr/bin/env scheme-script
;;; 1BRC — Chez Scheme (simple/idiomatic version)
;;; Usage: scheme --script 1brc.ss <measurements-file>

(import (chezscheme))

(define (format1 x)
  "Format a number with exactly one decimal place."
  (let* ((r (/ (round (* x 10.0)) 10.0))
         (s (number->string (exact->inexact r))))
    (let ((dot (string-index s #\.)))
      (if dot
          (let ((decimals (- (string-length s) dot 1)))
            (cond
              ((= decimals 1) s)
              ((> decimals 1) (substring s 0 (+ dot 2)))
              (else (string-append s "0"))))
          (string-append s ".0")))))

(define (string-index s ch)
  (let ((len (string-length s)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((char=? (string-ref s i) ch) i)
        (else (loop (+ i 1)))))))

(define (process-file path)
  (let ((ht (make-hashtable string-hash string=?))
        (port (open-input-file path)))
    (let loop ()
      (let ((line (get-line port)))
        (unless (eof-object? line)
          (let ((sep (string-index line #\;)))
            (when sep
              (let* ((name (substring line 0 sep))
                     (temp (string->number (substring line (+ sep 1) (string-length line))))
                     (entry (hashtable-ref ht name #f)))
                (if entry
                    (begin
                      (vector-set! entry 0 (min (vector-ref entry 0) temp))
                      (vector-set! entry 1 (max (vector-ref entry 1) temp))
                      (vector-set! entry 2 (+ (vector-ref entry 2) temp))
                      (vector-set! entry 3 (+ (vector-ref entry 3) 1)))
                    (hashtable-set! ht name (vector temp temp temp 1))))))
          (loop))))
    (close-input-port port)
    ht))

(define (format-output ht)
  (let* ((keys (vector->list (hashtable-keys ht)))
         (sorted (list-sort string<? keys)))
    (let ((parts
           (map (lambda (name)
                  (let* ((entry (hashtable-ref ht name #f))
                         (mn (vector-ref entry 0))
                         (mx (vector-ref entry 1))
                         (sm (vector-ref entry 2))
                         (ct (vector-ref entry 3))
                         (mean (/ sm ct)))
                    (string-append name "="
                                   (format1 mn) "/"
                                   (format1 mean) "/"
                                   (format1 mx))))
                sorted)))
      (string-append "{"
                     (let loop ((ps parts) (acc ""))
                       (cond
                         ((null? ps) acc)
                         ((null? (cdr ps)) (string-append acc (car ps)))
                         (else (loop (cdr ps) (string-append acc (car ps) ", ")))))
                     "}"))))

(define (current-ms)
  (let ((t (current-time)))
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

(let ((args (command-line-arguments)))
  (when (null? args)
    (display "Usage: scheme --script 1brc.ss <measurements-file>\n" (current-error-port))
    (exit 1))
  (let* ((path (car args))
         (t0 (current-ms))
         (ht (process-file path))
         (t1 (current-ms))
         (output (format-output ht))
         (t2 (current-ms)))
    (display output)
    (newline)
    (format (current-error-port) "Processed ~a stations in ~a ms\n"
            (hashtable-size ht) (- t1 t0))
    (format (current-error-port) "Total: ~a ms\n" (- t2 t0))))
