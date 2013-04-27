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

(defn after?
  [[x1 y1] [x2 y2]]
  (and
   (not= [x1 y1] [x2 y2])
   (>= x2 x1)
   (>= y2 y1)))

(defn before?
  [[x1 y1] [x2 y2]]
  (and
   (not= [x1 y1] [x2 y2])
   (<= x1 x2)
   (<= y1 y2)))

(defn stations-with-next-nodes
  [stations]
  (let [stations (sort-by (fn [[x y]] [(min x y) x y]) stations)]
    (loop [[current & remaining] stations
           mapping (transient {})]
      (if (nil? current)
        (persistent! mapping)
        (recur remaining (assoc! mapping
                                 current (loop [nexts () [f & r] (filter (partial after? current) remaining)]
                                           (if (nil? f)
                                             nexts
                                             (if (some #(before? % f) nexts)
                                               (recur nexts r)
                                               (recur (conj nexts f) r))))))))))

(defn longest-path
  [n]
  (let [stations   (into #{[0 0] [n n]} (generate-stations n))
        next-nodes (stations-with-next-nodes stations)]
    (loop [cnt 0
           positions (next-nodes [0 0])]
      (if (= positions #{[n n]})
        cnt
        (let [next-nodes (distinct (mapcat next-nodes positions))
              next-nodes (remove (fn [n] (some #(after? % n) next-nodes)) next-nodes)]
          (recur (inc cnt) (set next-nodes)))))))
