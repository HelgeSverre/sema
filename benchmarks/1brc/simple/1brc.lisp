;;; 1BRC — SBCL (simple/idiomatic version)
;;; Usage: sbcl --script 1brc.lisp /path/to/measurements.txt

(defstruct station
  (min 0.0d0)
  (max 0.0d0)
  (sum 0.0d0)
  (count 0))

(defun parse-temperature (string)
  "Parse a temperature string like \"-12.3\" into a double-float."
  (with-input-from-string (s string)
    (coerce (read s) 'double-float)))

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
         (file (or (car (last args))
                   (error "Usage: sbcl --script 1brc.lisp <file>")))
         (table (make-hash-table :test #'equal :size 512))
         (start-time (get-internal-real-time)))
    (with-open-file (stream file :direction :input
                                 :external-format :utf-8)
      (loop for line = (read-line stream nil nil)
            while line
            do (let* ((semi (position #\; line))
                      (name (subseq line 0 semi))
                      (temp (parse-temperature (subseq line (1+ semi))))
                      (entry (gethash name table)))
                 (if entry
                     (progn
                       (when (< temp (station-min entry)) (setf (station-min entry) temp))
                       (when (> temp (station-max entry)) (setf (station-max entry) temp))
                       (incf (station-sum entry) temp)
                       (incf (station-count entry)))
                     (setf (gethash name table)
                           (make-station :min temp :max temp :sum temp :count 1))))))
    ;; Collect and sort results
    (let ((results '()))
      (maphash (lambda (name entry)
                 (push (cons name entry) results))
               table)
      (setf results (sort results #'string< :key #'car))
      ;; Print
      (format t "{")
      (loop for ((name . entry) . rest) on results
            do (let ((mean (/ (station-sum entry)
                              (coerce (station-count entry) 'double-float))))
                 (format t "~A=~,1F/~,1F/~,1F"
                         name
                         (station-min entry)
                         mean
                         (station-max entry)))
               (when rest (format t ", ")))
      (format t "}~%"))
    ;; Timing
    (let* ((end-time (get-internal-real-time))
           (elapsed-ms (* 1000.0
                          (/ (- end-time start-time)
                             (coerce internal-time-units-per-second 'double-float)))))
      (format t "Elapsed: ~,1F ms~%" elapsed-ms))))

(main)
