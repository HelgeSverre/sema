(def start (os/clock))

(def path (get (dyn :args) 1))

# Read the whole file as a byte string and scan it manually.
# Janet strings are byte strings; (in s i) returns the byte (an integer),
# and string/slice is O(1), so we can parse without per-row allocations
# beyond the station name slice used as a table key.
(def data (slurp path))
(def n (length data))
(def stations @{})

# Byte constants
(def SEMI 59)   # ;
(def NL 10)     # \n
(def CR 13)     # \r
(def DOT 46)    # .
(def MINUS 45)  # -

(var i 0)
(while (< i n)
  # Find the ';' separating the station name from the temperature.
  (def name-start i)
  (var j i)
  (while (not= (in data j) SEMI)
    (set j (+ j 1)))
  (def name (string/slice data name-start j))

  # Parse the temperature as an integer scaled by 10 (temp*10),
  # scanning bytes until end-of-line. Handles a leading '-' and a
  # single '.' separator.
  (var k (+ j 1))
  (var neg false)
  (when (= (in data k) MINUS)
    (set neg true)
    (set k (+ k 1)))
  (var acc 0)
  (var b (in data k))
  (while (and (not= b NL) (not= b CR))
    (unless (= b DOT)
      (set acc (+ (* acc 10) (- b 48))))
    (set k (+ k 1))
    (if (< k n)
      (set b (in data k))
      (set b NL)))
  (def temp (if neg (- acc) acc))

  # Advance past the newline (and optional carriage return) to the next row.
  (while (and (< k n) (not= (in data k) NL))
    (set k (+ k 1)))
  (set i (+ k 1))

  (if-let [entry (get stations name)]
    (do
      (when (< temp (entry 0)) (put entry 0 temp))
      (when (> temp (entry 2)) (put entry 2 temp))
      (put entry 1 (+ (entry 1) temp))
      (put entry 3 (+ (entry 3) 1)))
    (put stations name @[temp temp temp 1])))

(def names (sorted (keys stations)))

# Format an integer scaled by 10 as a 1-decimal-place string,
# matching the Chicken reference (negatives and -0.x included).
(defn fmt [x]
  (def an (math/abs x))
  (def whole (div an 10))
  (def frac (mod an 10))
  (cond
    (and (< x 0) (= whole 0)) (string/format "-0.%d" frac)
    (< x 0) (string/format "-%d.%d" whole frac)
    (string/format "%d.%d" whole frac)))

(prin "{")
(for idx 0 (length names)
  (when (> idx 0) (prin ", "))
  (def name (get names idx))
  (def entry (get stations name))
  (def mn (entry 0))
  (def sum (entry 1))
  (def mx (entry 2))
  (def cnt (entry 3))
  # mean = round((sum*1.0)/cnt) in temp*10 units, matching 1brc.chicken.scm
  (def mean (math/round (/ (* sum 1.0) cnt)))
  (prin name "=" (fmt mn) "/" (fmt mean) "/" (fmt mx)))
(print "}")

(def elapsed (* (- (os/clock) start) 1000))
(eprintf "Elapsed: %.1f ms" elapsed)
