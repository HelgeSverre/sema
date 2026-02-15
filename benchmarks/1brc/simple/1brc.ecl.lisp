;;; 1BRC — ECL (simple/idiomatic version)
;;; Usage: ecl --shell 1brc.ecl.lisp /path/to/measurements.txt

(defun parse-temperature (string)
  "Parse a temperature string like \"-12.3\" into a float."
  (with-input-from-string (s string)
    (float (read s))))

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
                      (temp (parse-temperature (subseq line (1+ semi))))
                      (entry (gethash name table)))
                 (if entry
                     (progn
                       (when (< temp (aref entry 0)) (setf (aref entry 0) temp))
                       (when (> temp (aref entry 1)) (setf (aref entry 1) temp))
                       (incf (aref entry 2) temp)
                       (incf (aref entry 3)))
                     (setf (gethash name table)
                           (make-array 4 :initial-contents
                                       (list temp temp temp 1)))))))

    (let ((results '()))
      (maphash (lambda (name entry)
                 (push (cons name entry) results))
               table)
      (setf results (sort results #'string< :key #'car))

      (format t "{")
      (loop for ((name . entry) . rest) on results
            do (let ((mean (/ (aref entry 2) (aref entry 3))))
                 (format t "~A=~,1F/~,1F/~,1F"
                         name
                         (aref entry 0)
                         (float mean)
                         (aref entry 1)))
               (when rest (format t ", ")))
      (format t "}~%"))

    (let* ((end-time (get-internal-real-time))
           (elapsed-ms (* 1000.0
                          (/ (- end-time start-time)
                             (coerce internal-time-units-per-second 'double-float)))))
      (format *error-output* "Elapsed: ~,1F ms~%" elapsed-ms))))

(main)
(ext:quit 0)
