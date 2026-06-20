#lang racket/base

;; Parse [-]ddd.d (bytes) into integer temp ×10.
;; Reads from byte string `buf`, starting at index `start` (inclusive),
;; up to `end` (exclusive). Returns the signed integer ×10.
(define (parse-temp-int10 buf start end)
  (let ([b0 (bytes-ref buf start)])
    (if (= b0 45) ; #\-
        (- (parse-temp-int10 buf (add1 start) end))
        (let loop ([i start] [acc 0])
          (if (>= i end)
              acc
              (let ([c (bytes-ref buf i)])
                (if (= c 46) ; #\.
                    (loop (add1 i) acc)
                    (loop (add1 i) (+ (* acc 10) (- c 48))))))))))

(define (format-1dp n)
  (define abs-n (abs n))
  (define whole (quotient abs-n 10))
  (define frac (remainder abs-n 10))
  (cond
    [(and (< n 0) (= whole 0)) (format "-0.~a" frac)]
    [(< n 0) (format "-~a.~a" whole frac)]
    [else (format "~a.~a" whole frac)]))

(define (main)
  (define args (current-command-line-arguments))
  (when (= (vector-length args) 0)
    (eprintf "Usage: racket 1brc.rkt <file>\n")
    (exit 1))

  (define file-path (vector-ref args 0))
  (define start (current-inexact-milliseconds))

  ;; Accumulate: station (bytes) -> (vector min sum max count), all integers ×10
  (define table (make-hash))

  (call-with-input-file file-path
    (lambda (port)
      (define CHUNK (* 1 1024 1024))
      ;; carry holds a partial trailing line from the previous chunk.
      (let chunk-loop ([carry #""])
        (define raw (read-bytes CHUNK port))
        (cond
          [(eof-object? raw)
           ;; Process any final line without trailing newline.
           (when (> (bytes-length carry) 0)
             (process-line carry 0 (bytes-length carry) table))]
          [else
           (define buf (if (= (bytes-length carry) 0)
                           raw
                           (bytes-append carry raw)))
           (define len (bytes-length buf))
           ;; Scan for complete lines (terminated by newline).
           (let line-loop ([line-start 0])
             (define nl (find-byte buf line-start len 10)) ; newline
             (cond
               [nl
                (process-line buf line-start nl table)
                (line-loop (add1 nl))]
               [else
                ;; No more complete lines; carry the remainder.
                (chunk-loop (subbytes buf line-start len))]))]))))

  (define end (current-inexact-milliseconds))

  ;; Sort stations alphabetically (compare as strings for correct ordering).
  (define sorted-stations
    (sort (hash-keys table)
          (lambda (a b) (string<? (bytes->string/utf-8 a) (bytes->string/utf-8 b)))))

  (define parts
    (for/list ([station (in-list sorted-stations)])
      (define entry (hash-ref table station))
      (define mn (vector-ref entry 0))
      (define sum (vector-ref entry 1))
      (define mx (vector-ref entry 2))
      (define cnt (vector-ref entry 3))
      (define mean (inexact->exact (round (/ (* sum 1.0) cnt))))
      (format "~a=~a/~a/~a"
              (bytes->string/utf-8 station)
              (format-1dp mn)
              (format-1dp mean)
              (format-1dp mx))))

  (printf "{~a}\n" (string-join-bytes parts))
  (eprintf "Elapsed: ~a ms\n" (inexact->exact (round (- end start)))))

;; Find byte `target` in `buf` between [start, end). Returns index or #f.
(define (find-byte buf start end target)
  (let loop ([i start])
    (cond
      [(>= i end) #f]
      [(= (bytes-ref buf i) target) i]
      [else (loop (add1 i))])))

;; Process one line spanning [start, end) (excluding newline) in byte string buf.
(define (process-line buf start end table)
  (define semi (find-byte buf start end 59)) ; #\;
  (define station (subbytes buf start semi))
  (define temp (parse-temp-int10 buf (add1 semi) end))
  (define entry (hash-ref table station #f))
  (if entry
      (begin
        (when (< temp (vector-ref entry 0)) (vector-set! entry 0 temp))
        (vector-set! entry 1 (+ (vector-ref entry 1) temp))
        (when (> temp (vector-ref entry 2)) (vector-set! entry 2 temp))
        (vector-set! entry 3 (add1 (vector-ref entry 3))))
      (hash-set! table station (vector temp temp temp 1))))

;; Join strings with ", " (avoids requiring racket/string).
(define (string-join-bytes parts)
  (cond
    [(null? parts) ""]
    [else
     (apply string-append
            (car parts)
            (map (lambda (p) (string-append ", " p)) (cdr parts)))]))

(main)
