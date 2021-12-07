(defn read-line-segments []
  (->> file
  (slurp)
  (split-lines)
  (map (fn [line] (map #(vec %) (partition 2 (map #(Integer/parseInt %) (drop 1 (re-matches #"(\d*),(\d*) -> (\d*),(\d*)" line)))))))))

(defn build-line-seq [[[x1 y1] [x2 y2]]]
  (if (> x1 x2)
    (build-line-seq [[x2 y2] [x1 y1]])
    (if (= x1 x2)
      (map (fn [y] [x1 y]) (let [ys (sort [y1 y2])] (range (first ys) (+ 1 (second ys)))))
      (let [m (/ (- y2 y1) (- x2 x1))
            b (- y1 (* m x1))]
        (map (fn [x] [x (+ b (* m x))]) (range x1 (+ 1 x2)))))))

(defn count-intersecting-points [line-seqs]
  (count (filter #(> % 1) (vals (frequencies (apply concat line-seqs))))))

;; part one
(println (count-intersecting-points
    (map build-line-seq (filter (fn [[[x1 y1] [x2 y2]]] (or (= x1 x2) (= y1 y2))) (read-line-segments)))))

;; part two
(println (count-intersecting-points (map build-line-seq (read-line-segments))))
