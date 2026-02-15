;;; 1BRC — Clojure (simple/idiomatic version)
;;; Usage: clojure -M 1brc.clj /path/to/measurements.txt

(require '[clojure.java.io :as io]
         '[clojure.string :as str])

(let [file-path (first *command-line-args*)
      start     (System/currentTimeMillis)
      stats     (with-open [rdr (io/reader file-path)]
                  (reduce (fn [acc line]
                            (let [idx  (.indexOf ^String line (int \;))
                                  name (.substring ^String line 0 idx)
                                  temp (Double/parseDouble (.substring ^String line (inc idx)))]
                              (if-let [[mn sm mx cnt] (get acc name)]
                                (assoc acc name [(min mn temp) (+ sm temp) (max mx temp) (inc cnt)])
                                (assoc acc name [temp temp temp 1]))))
                          {}
                          (line-seq rdr)))
      sorted    (sort-by key stats)
      fmt       (fn [[mn sm mx cnt]]
                  (format "%.1f/%.1f/%.1f" mn (/ sm cnt) mx))
      entries   (map (fn [[k v]] (str k "=" (fmt v))) sorted)
      end       (System/currentTimeMillis)]
  (println (str "{" (str/join ", " entries) "}"))
  (println (str "Elapsed: " (- end start) " ms")))
