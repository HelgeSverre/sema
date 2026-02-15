;;; 1 Billion Row Challenge — ECL (Embeddable Common Lisp) implementation
;;; Usage: ecl --shell 1brc.ecl.lisp /path/to/measurements.txt

(defun parse-temp-int10 (line start)
  "Parse a temperature string like \"-12.3\" starting at position START in LINE.
   Returns an integer × 10 (e.g. \"-12.3\" → -123)."
  (let ((neg nil)
        (acc 0)
        (i start)
        (len (length line)))
    (when (and (< i len) (char= (char line i) #\-))
      (setf neg t)
      (incf i))
    (loop while (< i len)
          for ch = (char line i)
          do (cond
               ((char= ch #\.) (incf i))
               (t (setf acc (+ (* acc 10) (- (char-code ch) 48)))
                  (incf i))))
    (if neg (- acc) acc)))

(defun format-int10 (val)
  "Format an integer × 10 as a 1dp string (e.g. 123 → \"12.3\", -51 → \"-5.1\")."
  (let* ((neg (< val 0))
         (abs-val (abs val))
         (whole (floor abs-val 10))
         (frac (mod abs-val 10)))
    (if neg
        (format nil "-~D.~D" whole frac)
        (format nil "~D.~D" whole frac))))

(defun main ()
  (let* ((args (ext:command-args))
         (file (car (last args)))
         (table (make-hash-table :test #'equal :size 512))
         (start-time (get-internal-real-time)))

    (with-open-file (stream file :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((semi (position #\; line))
                      (name (subseq line 0 semi))
                      (temp (parse-temp-int10 line (1+ semi)))
                      (entry (gethash name table)))
                 (if entry
                     (progn
                       (when (< temp (aref entry 0)) (setf (aref entry 0) temp))
                       (when (> temp (aref entry 1)) (setf (aref entry 1) temp))
                       (incf (aref entry 2) temp)
                       (incf (aref entry 3)))
                     (let ((v (make-array 4 :element-type 'fixnum
                                            :initial-contents
                                            (list temp temp temp 1))))
                       (setf (gethash name table) v))))))

    (let ((results '()))
      (maphash (lambda (name entry)
                 (push (cons name entry) results))
               table)
      (setf results (sort results #'string< :key #'car))

      (format t "{")
      (loop for ((name . entry) . rest) on results
            do (let ((mean (round (/ (float (aref entry 2)) (aref entry 3)))))
                 (format t "~A=~A/~A/~A"
                         name
                         (format-int10 (aref entry 0))
                         (format-int10 mean)
                         (format-int10 (aref entry 1))))
               (when rest (format t ", ")))
      (format t "}~%"))

    (let* ((end-time (get-internal-real-time))
           (elapsed-ms (* 1000.0
                          (/ (- end-time start-time)
                             (coerce internal-time-units-per-second 'double-float)))))
      (format *error-output* "Elapsed: ~,1F ms~%" elapsed-ms))))

(main)
(ext:quit 0)
