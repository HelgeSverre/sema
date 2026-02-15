# 1BRC — Janet (simple/idiomatic version)
# Usage: janet 1brc.janet /path/to/measurements.txt

(def start (os/clock))

(def path (get (dyn :args) 1))
(def f (file/open path :r))
(def stations @{})

(while true
  (def line (file/read f :line))
  (unless line (break))
  (def sep (string/find ";" line))
  (def name (string/slice line 0 sep))
  (def raw (string/slice line (+ sep 1)))
  # Strip trailing newline/carriage return
  (def trimmed (string/trimr raw))
  (def temp (scan-number trimmed))
  (if-let [entry (get stations name)]
    (do
      (put entry :min (min (entry :min) temp))
      (put entry :max (max (entry :max) temp))
      (put entry :sum (+ (entry :sum) temp))
      (put entry :count (+ (entry :count) 1)))
    (put stations name @{:min temp :max temp :sum temp :count 1})))

(file/close f)

(def names (sorted (keys stations)))

(defn fmt [x]
  (string/format "%.1f" x))

(prin "{")
(for i 0 (length names)
  (when (> i 0) (prin ", "))
  (def name (get names i))
  (def entry (get stations name))
  (def mean (/ (entry :sum) (entry :count)))
  (prin name "=" (fmt (entry :min)) "/" (fmt mean) "/" (fmt (entry :max))))
(print "}")

(def elapsed (* (- (os/clock) start) 1000))
(eprintf "Elapsed: %.1f ms" elapsed)
