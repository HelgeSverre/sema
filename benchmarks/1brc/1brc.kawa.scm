;;; 1 Billion Row Challenge — Kawa Scheme (JVM) implementation
;;; Usage: kawa --script 1brc.kawa.scm /path/to/measurements.txt

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

(define (find-semicolon line)
  (let loop ((i 0))
    (cond
      ((= i (string-length line)) #f)
      ((char=? (string-ref line i) #\;) i)
      (else (loop (+ i 1))))))

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
  ;; (command-line) returns (java kawa.jar --script file.scm arg...)
  ;; The user arg is the last element
  (let* ((cl (command-line))
         (file-path (car (last-pair cl))))
    (when (or (not file-path) (string=? file-path (cadr cl)))
      (display "Usage: kawa --script 1brc.kawa.scm <file>\n"
               (current-error-port))
      (exit 1))

    (let* ((file-path file-path)
           (table (java.util.HashMap))
           (start-time (java.lang.System:currentTimeMillis))
           (reader (java.io.BufferedReader
                     (java.io.InputStreamReader
                       (java.io.FileInputStream file-path)))))

      (let loop ((line (reader:readLine)))
        (when (not (eq? line #!null))
          (let* ((semi (find-semicolon line))
                 (name (substring line 0 semi))
                 (temp (string->number (substring line (+ semi 1)
                                                  (string-length line))))
                 (entry (table:get name)))
            (if (not (eq? entry #!null))
                (begin
                  (vector-set! entry 0 (min (vector-ref entry 0) temp))
                  (vector-set! entry 1 (+ (vector-ref entry 1) temp))
                  (vector-set! entry 2 (max (vector-ref entry 2) temp))
                  (vector-set! entry 3 (+ (vector-ref entry 3) 1)))
                (table:put name (vector temp temp temp 1))))
          (loop (reader:readLine))))

      (reader:close)

      (let* ((keys-set (table:keySet))
             (names '()))
        (let ((iter (keys-set:iterator)))
          (let collect ()
            (when (iter:hasNext)
              (set! names (cons (iter:next) names))
              (collect))))
        (set! names (merge-sort names string<?))

        (let* ((parts (map (lambda (name)
                             (let* ((entry (table:get name))
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
               (end-time (java.lang.System:currentTimeMillis))
               (elapsed (- end-time start-time)))

          (display "{")
          (display (string-join parts ", "))
          (display "}")
          (newline)
          (display (string-append "Elapsed: "
                                  (number->string elapsed) " ms\n")
                   (current-error-port)))))))

(main)
