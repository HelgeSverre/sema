;;; 1 Billion Row Challenge — newLISP implementation
;;; Usage: newlisp 1brc.lsp /path/to/measurements.txt

;; Parse "[-]ddd.d" into an integer = temp * 10.
;; Drop the '.' so the digits read as temp*10, then parse as a base-10
;; integer (which carries the leading '-' for negatives, e.g. "-0.1" -> -1).
(define (parse-temp-int10 str)
  (int (replace "." str "") 0 10))

;; Format an int*10 value as a 1-decimal string, matching the Chicken impl.
(define (format-1dp n)
  (let (abs-n (abs n)
        whole 0
        frac 0)
    (setq whole (/ abs-n 10))
    (setq frac (% abs-n 10))
    (cond
      ((and (< n 0) (= whole 0)) (append "-0." (string frac)))
      ((< n 0) (append "-" (string whole) "." (string frac)))
      (true (append (string whole) "." (string frac))))))

(define (main)
  (let (file-path (main-args 2))
    (unless file-path
      (write-line 2 "Usage: newlisp 1brc.lsp <file>")
      (exit 1))

    (new Tree 'H)
    (let (start-time (time-of-day)
          fd (open file-path "r"))

      (unless fd
        (write-line 2 (append "Cannot open: " file-path))
        (exit 1))

      ;; Read and accumulate (state per station: (min sum max count), all int*10)
      (while (read-line fd)
        (letn (line (current-line)
               semi (find ";" line))
          (when semi
            (letn (name (slice line 0 semi)
                   temp (parse-temp-int10 (slice line (+ semi 1)))
                   entry (H name))
              (if entry
                  (H name (list (min (entry 0) temp)
                                (+ (entry 1) temp)
                                (max (entry 2) temp)
                                (+ (entry 3) 1)))
                  (H name (list temp temp temp 1)))))))

      (close fd)

      ;; Sort station names, build output
      (let (names (sort (map first (H)))
            parts '())
        (dolist (name names)
          (letn (entry (H name)
                 mn (entry 0)
                 sum (entry 1)
                 mx (entry 2)
                 cnt (entry 3)
                 mean (int (round (div (mul sum 1.0) cnt))))
            (push (append name "="
                          (format-1dp mn) "/"
                          (format-1dp mean) "/"
                          (format-1dp mx))
                  parts -1)))
        (println "{" (join parts ", ") "}"))

      (let (elapsed (- (time-of-day) start-time))
        (write-line 2 (append "Elapsed: " (string elapsed) " ms"))))))

(main)
(exit)
