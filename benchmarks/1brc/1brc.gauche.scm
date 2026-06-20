;;; 1 Billion Row Challenge — Gauche Scheme implementation (byte-oriented).
;;; Usage: gosh 1brc.gauche.scm /path/to/measurements.txt
;;;
;;; Gauche stores strings as UTF-8 and indexes by character, so the naive
;;; read-line + substring + string->number version pays UTF-8 navigation on
;;; every slice. This version reads the whole file into a u8vector and scans
;;; bytes directly, parsing temperatures as integer×10 (no float parse, no
;;; per-row temp substring) — the same shape as the SBCL/Chicken fast paths.

(use gauche.uvector)
(use file.util)   ; file-size
(use scheme.sort) ; list-sort
(use srfi.13)     ; string-join

(define (format-1dp n) ; n is temp×10 (integer)
  (let* ((a (abs n)) (whole (quotient a 10)) (frac (remainder a 10)))
    (cond
      ((and (< n 0) (= whole 0)) (format #f "-0.~a" frac))
      ((< n 0) (format #f "-~a.~a" whole frac))
      (else (format #f "~a.~a" whole frac)))))

(define (main args)
  (when (< (length args) 2)
    (format (current-error-port) "Usage: gosh 1brc.gauche.scm <file>~%")
    (exit 1))

  (let* ((path (cadr args))
         (size (file-size path))
         (buf (make-u8vector size))
         (table (make-hash-table 'string=?)))

    ;; Slurp the whole file into the byte buffer.
    (call-with-input-file path
      (lambda (p)
        (let rd ((off 0))
          (when (< off size)
            (let ((n (read-uvector! buf p off)))
              (when (and (integer? n) (> n 0))
                (rd (+ off n))))))))

    ;; Byte scan: Name ';' [-]d+.d '\n'  → station → vector(min sum max count), temps ×10.
    (let loop ((i 0))
      (when (< i size)
        (let scan ((j i))
          (if (= (u8vector-ref buf j) 59) ; #\;
              (let ((name (u8vector->string buf i j))
                    (neg (= (u8vector-ref buf (+ j 1)) 45))) ; #\-
                (let dloop ((m (if neg (+ j 2) (+ j 1))) (acc 0))
                  (let ((b (if (< m size) (u8vector-ref buf m) 10)))
                    (cond
                      ((= b 46) (dloop (+ m 1) acc)) ; #\.
                      ((and (>= b 48) (<= b 57))
                       (dloop (+ m 1) (+ (* acc 10) (- b 48))))
                      (else ; #\newline or EOF
                       (let ((temp (if neg (- acc) acc))
                             (e (hash-table-get table name #f)))
                         (if e
                             (begin
                               (vector-set! e 0 (min (vector-ref e 0) temp))
                               (vector-set! e 1 (+ (vector-ref e 1) temp))
                               (vector-set! e 2 (max (vector-ref e 2) temp))
                               (vector-set! e 3 (+ (vector-ref e 3) 1)))
                             (hash-table-put! table name (vector temp temp temp 1)))
                         (loop (+ m 1)))))))) ; resume after the newline
              (scan (+ j 1))))))

    (let* ((names (list-sort string<? (hash-table-keys table)))
           (parts (map (lambda (nm)
                         (let* ((e (hash-table-get table nm))
                                (mn (vector-ref e 0))
                                (sum (vector-ref e 1))
                                (mx (vector-ref e 2))
                                (cnt (vector-ref e 3))
                                (mean (inexact->exact (round (/ (* sum 1.0) cnt)))))
                           (string-append nm "="
                                          (format-1dp mn) "/"
                                          (format-1dp mean) "/"
                                          (format-1dp mx))))
                       names)))
      (format #t "{~a}~%" (string-join parts ", ")))
    0)) ; main's return value is the process exit code in Gauche

; Gauche auto-calls (main) at exit when defined
