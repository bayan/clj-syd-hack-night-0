(ns clj-syd.core
  (:gen-class ))

(defn mod-pow [base i n]
  (.intValue (.modPow (BigInteger/valueOf base)
                      (BigInteger/valueOf i)
                      (BigInteger/valueOf n))))

(defn generate-stations
  [n]
  (let [generator (fn [i] [(mod-pow 2 i n) (mod-pow 3 i n)])]
    (distinct (map generator (range 0 (inc (* 2 n)))))))

(defn next-points
  [[x y] stations]
  (filter
   (fn [[s-x s-y]]
     (and (>= s-x x)
          (>= s-y y)
          (not= [x y] [s-x s-y])))
   stations))

(defn longest-paths
  [n]
  (loop [paths [{:path [[0 0]]
                 :remaining-points (conj (generate-stations n) [n n]) }]]
    (let [paths-with-more-steps (filter (fn [path] (> (count (path :remaining-points)) 0)) paths)]
      (if (zero? (count paths-with-more-steps))
        (map :path paths)
        (recur
         (mapcat
          (fn [{:keys [path remaining-points]}]
            (let [candidates (next-points (last path) remaining-points)]
              (map
               (fn [candidate]
                 {:path (conj path candidate)
                  :remaining-points (next-points candidate candidates)})
               candidates)))
          paths-with-more-steps))))))

(defn longest-path-length [n]
  (- (count (first (longest-paths n))) 2))

(defn -main
  "Project E*l*r - Problem 411"
  [& args]
  (println "Stations: " (generate-stations 22)))
