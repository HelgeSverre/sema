;;; 1 Billion Row Challenge — ECL (Embeddable Common Lisp) implementation
;;; Usage: ecl --shell 1brc.ecl.lisp /path/to/measurements.txt

(defun format-1dp (x)
  (format nil "~,1F" x))

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
                      (temp (read-from-string (subseq line (1+ semi))))
                      (temp-f (coerce temp 'double-float))
                      (entry (gethash name table)))
                 (if entry
                     (progn
                       (when (< temp-f (aref entry 0)) (setf (aref entry 0) temp-f))
                       (when (> temp-f (aref entry 1)) (setf (aref entry 1) temp-f))
                       (incf (aref entry 2) temp-f)
                       (incf (aref entry 3)))
                     (let ((v (make-array 4 :initial-contents
                                          (list temp-f temp-f temp-f 1))))
                       (setf (gethash name table) v))))))

    (let ((results '()))
      (maphash (lambda (name entry)
                 (push (cons name entry) results))
               table)
      (setf results (sort results #'string< :key #'car))

      (format t "{")
      (loop for ((name . entry) . rest) on results
            do (let ((mean (/ (aref entry 2)
                              (coerce (aref entry 3) 'double-float))))
                 (format t "~A=~A/~A/~A"
                         name
                         (format-1dp (aref entry 0))
                         (format-1dp mean)
                         (format-1dp (aref entry 1))))
               (when rest (format t ", ")))
      (format t "}~%"))

    (let* ((end-time (get-internal-real-time))
           (elapsed-ms (* 1000.0
                          (/ (- end-time start-time)
                             (coerce internal-time-units-per-second 'double-float)))))
      (format *error-output* "Elapsed: ~,1F ms~%" elapsed-ms))))

(main)
(ext:quit 0)
