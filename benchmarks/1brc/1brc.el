;;; 1brc.el --- 1 Billion Row Challenge -*- lexical-binding: t; -*-
;;; Usage: emacs --batch --script 1brc.el /path/to/measurements.txt

(defun 1brc-main ()
  (let ((file-path (car command-line-args-left)))
    (unless file-path
      (message "Usage: emacs --batch --script 1brc.el <file>")
      (kill-emacs 1))
    (setq command-line-args-left (cdr command-line-args-left))

    (let ((table (make-hash-table :test 'equal :size 512))
          (start-time (float-time)))

      ;; Load file into buffer and process in-place (no string extraction)
      (with-temp-buffer
        (insert-file-contents-literally file-path)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line-start (point))
                semi-pos name temp entry)
            ;; Find semicolon by scanning buffer directly
            (setq semi-pos (search-forward ";" nil t))
            (when semi-pos
              ;; Station name: from line-start to just before ";"
              (setq name (buffer-substring-no-properties line-start (1- semi-pos)))
              ;; Temperature: parse from current point to end of line
              ;; Manual integer*10 parser to avoid float overhead
              (let ((neg nil)
                    (result 0)
                    (ch (char-after)))
                (when (eq ch ?-)
                  (setq neg t)
                  (forward-char 1)
                  (setq ch (char-after)))
                (while (and ch (not (eq ch ?\n)))
                  (if (eq ch ?.)
                      nil ; skip decimal point
                    (setq result (+ (* result 10) (- ch ?0))))
                  (forward-char 1)
                  (setq ch (char-after)))
                (when neg (setq result (- result)))
                ;; result is temp * 10 as integer
                (setq entry (gethash name table))
                (if entry
                    (progn
                      (when (< result (aref entry 0)) (aset entry 0 result))
                      (when (> result (aref entry 1)) (aset entry 1 result))
                      (aset entry 2 (+ (aref entry 2) result))
                      (aset entry 3 (1+ (aref entry 3))))
                  (puthash name (vector result result result 1) table)))))
          (forward-line 1)))

      ;; Collect and sort
      (let ((names '()))
        (maphash (lambda (k _v) (push k names)) table)
        (setq names (sort names #'string<))

        ;; Format output — convert integer*10 back to 1dp
        (let ((parts
               (mapcar (lambda (name)
                         (let* ((entry (gethash name table))
                                (mn (aref entry 0))
                                (mx (aref entry 1))
                                (sum (aref entry 2))
                                (cnt (aref entry 3))
                                (mean (round (/ (float sum) cnt))))
                           (format "%s=%s/%s/%s"
                                   name
                                   (1brc-fmt mn)
                                   (1brc-fmt mean)
                                   (1brc-fmt mx))))
                       names)))

          (princ (format "{%s}\n" (mapconcat #'identity parts ", ")))
          (let* ((end-time (float-time))
                 (elapsed (round (* (- end-time start-time) 1000))))
            (message "Elapsed: %d ms" elapsed)))))))

(defun 1brc-fmt (n)
  "Format integer*10 N as string with 1 decimal place."
  (let* ((neg (< n 0))
         (abs-n (abs n))
         (whole (/ abs-n 10))
         (frac (% abs-n 10)))
    (if neg
        (format "-%d.%d" whole frac)
      (format "%d.%d" whole frac))))

(1brc-main)
