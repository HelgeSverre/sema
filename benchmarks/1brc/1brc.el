;;; 1 Billion Row Challenge — Emacs Lisp implementation
;;; Usage: emacs --batch --script 1brc.el /path/to/measurements.txt

(defun format-1dp (x)
  "Format number X to 1 decimal place."
  (format "%.1f" x))

(defun 1brc-main ()
  (let ((file-path (car command-line-args-left)))
    (unless file-path
      (message "Usage: emacs --batch --script 1brc.el <file>")
      (kill-emacs 1))
    (setq command-line-args-left (cdr command-line-args-left))

    (let ((table (make-hash-table :test 'equal))
          (start-time (float-time)))

      (with-temp-buffer
        (insert-file-contents file-path)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (line (buffer-substring-no-properties line-start line-end))
                 (semi (string-search ";" line)))
            (when semi
              (let* ((name (substring line 0 semi))
                     (temp (string-to-number (substring line (1+ semi))))
                     (entry (gethash name table)))
                (if entry
                    (progn
                      (when (< temp (aref entry 0)) (aset entry 0 temp))
                      (when (> temp (aref entry 1)) (aset entry 1 temp))
                      (aset entry 2 (+ (aref entry 2) temp))
                      (aset entry 3 (1+ (aref entry 3))))
                  (puthash name (vector temp temp temp 1) table)))))
          (forward-line 1)))

      ;; Collect and sort
      (let ((names '()))
        (maphash (lambda (k _v) (push k names)) table)
        (setq names (sort names #'string<))

        (let ((parts
               (mapcar (lambda (name)
                         (let* ((entry (gethash name table))
                                (mn (aref entry 0))
                                (mx (aref entry 1))
                                (sum (aref entry 2))
                                (cnt (aref entry 3))
                                (mean (/ sum cnt)))
                           (format "%s=%s/%s/%s"
                                   name
                                   (format-1dp mn)
                                   (format-1dp mean)
                                   (format-1dp mx))))
                       names)))

          (princ (format "{%s}\n" (mapconcat #'identity parts ", ")))
          (let* ((end-time (float-time))
                 (elapsed (round (* (- end-time start-time) 1000))))
            (message "Elapsed: %d ms" elapsed)))))))

(1brc-main)
