;;; 1BRC — newLISP (simple/idiomatic version)
;;; Usage: newlisp 1brc.lsp /path/to/measurements.txt

(define (format-1dp x)
  (format "%.1f" x))

(define (main)
  (let (file-path (main-args 2))
    (unless file-path
      (write-line 2 "Usage: newlisp 1brc.lsp <file>")
      (exit 1))

    (let (table '()
          start-time (time-of-day)
          fd (open file-path "r"))

      (unless fd
        (write-line 2 (append "Cannot open: " file-path))
        (exit 1))

      ;; Read and accumulate
      (while (read-line fd)
        (let (line (current-line))
          (let (semi (find ";" line))
            (when semi
              (let (name (slice line 0 semi))
                (let (temp (float (slice line (+ semi 1))))
                  (let (entry (assoc name table))
                    (if entry
                        (setf (assoc name table)
                              (list name
                                    (min (entry 1) temp)
                                    (max (entry 2) temp)
                                    (add (entry 3) temp)
                                    (+ (entry 4) 1)))
                        (push (list name temp temp temp 1) table -1)))))))))

      (close fd)

      ;; Sort by station name
      (sort table (fn (a b) (< (a 0) (b 0))))

      ;; Format output
      (let (parts (map (fn (e)
                         (let (name (e 0))
                           (let (mn (e 1))
                             (let (mx (e 2))
                               (let (sum (e 3))
                                 (let (cnt (e 4))
                                   (let (mean (div sum cnt))
                                     (append name "="
                                             (format-1dp mn) "/"
                                             (format-1dp mean) "/"
                                             (format-1dp mx)))))))))
                       table))
        (println "{" (join parts ", ") "}"))

      (let (elapsed (- (time-of-day) start-time))
        (write-line 2 (append "Elapsed: " (string elapsed) " ms"))))))

(main)
(exit)
