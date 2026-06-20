;;; 1 Billion Row Challenge — Fennel (Lua/LuaJIT) implementation
;;; Usage: fennel --lua luajit 1brc.fnl /path/to/measurements.txt
;;;
;;; Performance notes: temperatures are parsed directly from the read buffer as
;;; integers ×10 via string.byte (no substring, no generic tonumber), and
;;; min/sum/max/count are accumulated as integers ×10. Input is consumed in
;;; large blocks rather than line-by-line. Output formatting divides by 10 and
;;; uses "%.1f" — identical rounding to the original tonumber/%.1f path.

(local file-path (. arg 1))

(when (not file-path)
  (io.stderr:write "Usage: fennel --lua luajit 1brc.fnl <file>\n")
  (os.exit 1))

(local start-time (os.clock))
(local stations {})

;; Localize hot functions for LuaJIT.
(local sbyte string.byte)
(local ssub string.sub)
(local sfind string.find)

(local f (assert (io.open file-path :r)))

;; Process one line given as the substring [from, to] (1-based inclusive) of buf.
;; Parses "Name;[-]DD.D" with the temperature stored as an integer ×10.
(fn process [buf from to]
  (let [semi (sfind buf ";" from true)
        name (ssub buf from (- semi 1))]
    ;; Parse temperature (integer ×10) directly from bytes.
    (var i (+ semi 1))
    (var neg false)
    (when (= (sbyte buf i) 45) ; '-'
      (set neg true)
      (set i (+ i 1)))
    (var temp 0)
    (while (<= i to)
      (let [b (sbyte buf i)]
        (when (not= b 46) ; skip '.'
          (set temp (+ (* temp 10) (- b 48))))
        (set i (+ i 1))))
    (when neg (set temp (- temp)))
    (let [entry (. stations name)]
      (if entry
          (do
            (when (< temp (. entry 1)) (tset entry 1 temp))
            (when (> temp (. entry 3)) (tset entry 3 temp))
            (tset entry 2 (+ (. entry 2) temp))
            (tset entry 4 (+ (. entry 4) 1)))
          (tset stations name [temp temp temp 1])))))

;; Read in large blocks; carry any trailing partial line into the next block.
(local block-size (* 1024 1024))
(var pending "")
(var chunk (f:read block-size))
(while chunk
  (let [buf (.. pending chunk)
        len (length buf)]
    (var pos 1)
    (var nl (sfind buf "\n" pos true))
    (while nl
      (process buf pos (- nl 1))
      (set pos (+ nl 1))
      (set nl (sfind buf "\n" pos true)))
    (set pending (if (<= pos len) (ssub buf pos len) "")))
  (set chunk (f:read block-size)))

;; Handle a final line with no trailing newline.
(when (> (length pending) 0)
  (process pending 1 (length pending)))

(f:close)

;; Collect and sort station names
(local names [])
(each [name _ (pairs stations)]
  (table.insert names name))
(table.sort names)

;; Format output. min/max/mean divided by 10 and printed with %.1f, matching the
;; original tonumber/%.1f rounding exactly.
(local parts [])
(each [_ name (ipairs names)]
  (let [entry (. stations name)
        mn (. entry 1)
        sum (. entry 2)
        mx (. entry 3)
        cnt (. entry 4)
        mean (/ (/ sum 10) cnt)]
    (table.insert parts
                  (string.format "%s=%.1f/%.1f/%.1f" name (/ mn 10) mean (/ mx 10)))))

(io.write (.. "{" (table.concat parts ", ") "}\n"))

(local end-time (os.clock))
(local elapsed (* (- end-time start-time) 1000))
(io.stderr:write (string.format "Elapsed: %.0f ms\n" elapsed))
