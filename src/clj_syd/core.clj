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

(defn new-path
  [stations remaining]
  { :stations stations :remaining remaining })

(defn prune [paths]
  (vals
   (reduce
    (fn [longest-paths path]
      (let [station (last (path :stations))
            current-longest (longest-paths station)]
        (if (and current-longest
                 (> (count (current-longest :stations))
                    (count (path :stations))))
          longest-paths
          (assoc longest-paths station path))))
    {}
    paths)))

(defn branch-path
  [path remaining-points]
  (let [candidates (next-points (last path) remaining-points)]
    (map (fn [candidate]
           (new-path (conj path candidate)
                     (next-points candidate candidates)))
         candidates)))

(defn branch-all-paths
  [paths]
  (prune
   (mapcat
    (fn [{:keys [stations remaining]}]
      (branch-path stations remaining))
    paths)))

(defn longest-paths
  [n]
  (map :stations
       (ffirst (drop-while (fn [[_ path]] (not (empty? path)))
                           (partition 2 1 (iterate branch-all-paths
                                                   [(new-path [[0 0]]
                                                              (conj (generate-stations n) [n n]))]))))))
(defn longest-path-length [n]
  (-> n
      longest-paths
      first
      count
      (- 2))) ;; ignore [0 0] and [n n] stations

(defn -main
  "Project E*l*r - Problem 411"
  [& args]
  (println "Stations: " (generate-stations 22)))
