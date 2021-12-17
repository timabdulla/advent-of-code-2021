(ns aoc.day-11
  (:require [clojure.string :refer [split split-lines join]]
            [clojure.set :refer [difference union intersection]]
            [clojure.core.matrix :refer [non-zero-indices gt mget shape ereduce add emap-indexed non-zero-count zero-count]]))

(def file "day_11.txt")

(defn parse-input-file []
  (let [lines (split-lines (slurp file))]
    (vec (map (fn [line] (vec (map #(Integer/parseInt %) (split line #"")))) lines))))

(defn neighbours [mat [i j]]
  (let [[max-i max-j] (shape mat)]
    (for [n-i (map #(+ i %) (range -1 2))
          n-j (map #(+ j %) (range -1 2))
          :when (and (not (= [i j] [n-i n-j])) (>= n-i 0) (< n-i max-i) (>= n-j 0) (< n-j max-j))]
      [n-i n-j])))

(defn run-step [mat]
  (loop [mat (add mat 1) already-flashed #{} tries 0]
    (let [flashed-idxs (apply concat (map-indexed (fn [i js] (map (fn [j] [i j]) js)) (non-zero-indices (gt mat 9))))
          neighbours-without-flashed (fn [mat idx] (difference (set (neighbours mat idx)) already-flashed (set flashed-idxs)))
          [updates newly-flashed] (reduce (fn [[u f] idx]
                                               [(assoc (reduce #(update %1 %2 (fnil + 0) 1) u (neighbours-without-flashed mat idx)) idx (- (apply mget mat idx))) (conj f idx)])
                                           [{} #{}] flashed-idxs)]
      (if (empty? newly-flashed)
        mat
        (recur (emap-indexed #(+ %2 (get updates %1 0)) mat) (union newly-flashed already-flashed) (inc tries))))))

(defn part-one []
  (reduce + (map zero-count (take 101 (iterate run-step (parse-input-file))))))

(defn part-two []
  (count (take-while #(> (non-zero-count %) 0) (iterate run-step (parse-input-file)))))
