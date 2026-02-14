;;; 1 Billion Row Challenge — SBCL implementation
;;; Usage: sbcl --script 1brc.lisp /path/to/measurements.txt

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defstruct (station (:constructor make-station (min max sum count)))
  (min 0.0d0 :type double-float)
  (max 0.0d0 :type double-float)
  (sum 0.0d0 :type double-float)
  (count 0   :type fixnum))

(declaim (inline parse-temperature))
(defun parse-temperature (line start end)
  "Parse a temperature value from LINE between START and END.
   Format is always [-]digits[.digit]. Returns an integer representing temp * 10."
  (declare (type simple-string line)
           (type fixnum start end)
           (optimize (speed 3) (safety 0)))
  (let ((neg nil)
        (idx start)
        (result 0))
    (declare (type fixnum idx result))
    (when (char= (schar line idx) #\-)
      (setf neg t)
      (incf idx))
    (loop while (< idx end)
          for ch of-type character = (schar line idx)
          do (if (char= ch #\.)
                 (incf idx)
                 (progn
                   (setf result (+ (* result 10)
                                   (- (char-code ch) #.(char-code #\0))))
                   (incf idx))))
    (if neg (- result) result)))

(defun main ()
  (let* ((args sb-ext:*posix-argv*)
         (file (or (car (last args))
                   (error "Usage: sbcl --script 1brc.lisp <file>")))
         (table (make-hash-table :test #'equal :size 512))
         (start-time (get-internal-real-time)))
    (with-open-file (stream file :direction :input
                                 :element-type 'character
                                 :external-format :utf-8)
      (let ((buf-size (* 1024 1024))
            (leftover ""))
        (declare (type simple-string leftover))
        (let ((buf (make-string buf-size :element-type 'character)))
          (loop
            (let ((n (read-sequence buf stream)))
              (declare (type fixnum n))
              (when (zerop n)
                ;; Process any remaining leftover
                (when (plusp (length leftover))
                  (let* ((line leftover)
                         (semi (position #\; line)))
                    (when semi
                      (let* ((name (subseq line 0 semi))
                             (temp-int (parse-temperature line (1+ semi) (length line)))
                             (temp (/ (coerce temp-int 'double-float) 10.0d0))
                             (entry (gethash name table)))
                        (if entry
                            (progn
                              (when (< temp (station-min entry)) (setf (station-min entry) temp))
                              (when (> temp (station-max entry)) (setf (station-max entry) temp))
                              (incf (station-sum entry) temp)
                              (incf (station-count entry)))
                            (setf (gethash name table) (make-station temp temp temp 1)))))))
                (return))
              ;; Combine leftover with new chunk
              (let* ((chunk (if (plusp (length leftover))
                                (concatenate 'string leftover (subseq buf 0 n))
                                (subseq buf 0 n)))
                     (chunk-len (length chunk))
                     (pos 0))
                (declare (type fixnum chunk-len pos)
                         (type simple-string chunk))
                (loop
                  (let ((nl (position #\Newline chunk :start pos)))
                    (unless nl (return))
                    (let ((semi (position #\; chunk :start pos :end nl)))
                      (when semi
                        (let* ((name (subseq chunk pos semi))
                               (temp-int (parse-temperature chunk (1+ semi) nl))
                               (temp (/ (coerce temp-int 'double-float) 10.0d0))
                               (entry (gethash name table)))
                          (if entry
                              (progn
                                (when (< temp (station-min entry)) (setf (station-min entry) temp))
                                (when (> temp (station-max entry)) (setf (station-max entry) temp))
                                (incf (station-sum entry) temp)
                                (incf (station-count entry)))
                              (setf (gethash name table) (make-station temp temp temp 1))))))
                    (setf pos (1+ nl))))
                (setf leftover (if (< pos chunk-len)
                                   (subseq chunk pos chunk-len)
                                   ""))))))))
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
