# tak.janet — TAKeuchi function benchmark
# Equivalent to tak.sema for cross-language comparison.

(defn tak [x y z]
  (if (not (< y x))
    z
    (tak (tak (- x 1) y z)
         (tak (- y 1) z x)
         (tak (- z 1) x y))))

(var result 0)
(for n 0 500
  (set result (tak 18 12 6)))
(print result)
