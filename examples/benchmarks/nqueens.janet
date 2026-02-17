# nqueens.janet — N-Queens solver benchmark
# Equivalent to nqueens.sema for cross-language comparison.

(defn one-to [n]
  (def result @[])
  (for i 1 (+ n 1)
    (array/push result i))
  (tuple/slice result))

(defn ok? [row dist placed]
  (if (= (length placed) 0)
    true
    (let [first-placed (placed 0)
          rest-placed (tuple/slice placed 1)]
      (and (not= first-placed (+ row dist))
           (not= first-placed (- row dist))
           (ok? row (+ dist 1) rest-placed)))))

(defn try-it [x y z]
  (if (= (length x) 0)
    (if (= (length y) 0) 1 0)
    (let [first-x (x 0)
          rest-x (tuple/slice x 1)]
      (+ (if (ok? first-x 1 z)
           (try-it [;rest-x ;y] [] [first-x ;z])
           0)
         (try-it rest-x [first-x ;y] z)))))

(defn nqueens [n]
  (try-it (one-to n) [] []))

(var result 0)
(for n 0 500
  (set result (nqueens 8)))
(print result)
