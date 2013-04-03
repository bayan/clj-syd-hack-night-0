(ns clj-syd.core
  (:gen-class ))

(defn mod-pow
  [base i n]
  (.intValue (.modPow (BigInteger/valueOf base)
                      (BigInteger/valueOf i)
                      (BigInteger/valueOf n))))

(defn generate-station
  [n i]
  [(mod-pow 2 i n)
   (mod-pow 3 i n)])

(defn generate-stations
  [n]
  (set
   (map (partial generate-station n)
        (range 0 (inc (* 2 n))))))

(defn generate-sorted-stations
  [n]
  (sort-by (fn [[x y]] [(min x y) x y])
           (generate-stations n)))

(defn after?
  [[x1 y1] [x2 y2]]
  (and
   (not= [x1 y1] [x2 y2])
   (>= x2 x1)
   (>= y2 y1)))

(defn expand-with-station
  [paths station]
  (reduce (fn [all [last-station path-length]]
            (if (after? last-station station)
              (conj all [station (inc path-length)])
              all))
          paths
          paths))

(defn expand-with-station-and-prune
  [paths station]
  (seq
   (persistent!
    (reduce
     (fn [all [last-station path-length]]
       (let [current-max (all last-station)]
         (if (< path-length (or current-max 0))
           all
           (assoc! all last-station path-length))))
     (transient {})
     (expand-with-station paths station)))))


(defn longest-path-length-for
  [n]
  (apply max
         (map last
              (reduce expand-with-station-and-prune
                      [[[0 0] 0]]
                      (generate-sorted-stations n)))))

(defn -main
  "Project E*l*r - Problem 411"
  [& args]
  (println "Stations: " (generate-stations 22)))
