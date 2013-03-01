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

(def branch-out
  (fn [path remaining-points]
    (let [candidates (next-points (last path) remaining-points)]
      (map (fn [candidate] { :path (conj path candidate) :remaining-points (next-points candidate candidates)})
           candidates))))

(defn prune [paths]
  (vals
   (reduce
    (fn [longest-paths path]
      (let [station (last (path :path))
            current-longest (longest-paths station)]
        (if (and current-longest
                 (> (count (current-longest :path))
                    (count (path :path))))
          longest-paths
          (assoc longest-paths station path))))
    {}
    paths)))


(defn longest-paths
  [n]
  (map :path
       (ffirst (drop-while (fn [[_ path]] (not (empty? path)))
                           (partition 2 1
                                      (iterate
                                       (fn [paths]
                                         (prune
                                          (mapcat
                                           (fn [{:keys [path remaining-points]}]
                                             (branch-out path remaining-points))
                                           paths)))
                                       [{:path [[0 0]] :remaining-points (conj (generate-stations n) [n n]) }]))))))


(defn longest-path-length [n]
  (- (count (first (longest-paths n))) 2))

(defn -main
  "Project E*l*r - Problem 411"
  [& args]
  (println "Stations: " (generate-stations 22)))
