;;; 1BRC — Fennel (simple/idiomatic version)
;;; Usage: fennel --lua luajit 1brc.fnl /path/to/measurements.txt

(local file-path (. arg 1))

(when (not file-path)
  (io.stderr:write "Usage: fennel --lua luajit 1brc.fnl <file>\n")
  (os.exit 1))

(local start-time (os.clock))
(local stations {})

(local f (assert (io.open file-path :r)))

(var line (f:read :*l))
(while line
  (let [semi (string.find line ";" 1 true)
        name (string.sub line 1 (- semi 1))
        temp (tonumber (string.sub line (+ semi 1)))]
    (let [entry (. stations name)]
      (if entry
          (do
            (when (< temp (. entry :min)) (tset entry :min temp))
            (when (> temp (. entry :max)) (tset entry :max temp))
            (tset entry :sum (+ (. entry :sum) temp))
            (tset entry :count (+ (. entry :count) 1)))
          (tset stations name {:min temp :max temp :sum temp :count 1}))))
  (set line (f:read :*l)))

(f:close)

;; Collect and sort station names
(local names [])
(each [name _ (pairs stations)]
  (table.insert names name))
(table.sort names)

;; Format output
(local parts [])
(each [_ name (ipairs names)]
  (let [entry (. stations name)
        mn (. entry :min)
        mx (. entry :max)
        sum (. entry :sum)
        cnt (. entry :count)
        mean (/ sum cnt)]
    (table.insert parts
                  (string.format "%s=%.1f/%.1f/%.1f" name mn mean mx))))

(io.write (.. "{" (table.concat parts ", ") "}\n"))

(local end-time (os.clock))
(local elapsed (* (- end-time start-time) 1000))
(io.stderr:write (string.format "Elapsed: %.0f ms\n" elapsed))
